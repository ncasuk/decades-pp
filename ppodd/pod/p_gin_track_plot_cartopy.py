import ppodd
from ppodd.core import *
import os.path
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import numpy as np

class gin_track_plot_cartopy(cal_base):
    """
    """
    def __init__(self,dataset):
        self.input_names=['DATE','FLIGHT','GINDAT_lat','GINDAT_lon']
        self.outputs=[constants_parameter('MAP_FILE',None)]
        cal_base.__init__(self,dataset)
    def process(self):
        lat='GINDAT_lat'
        lon='GINDAT_lon'
        cont_color='coral'
        trackcolor='green'
        trackwidth=2.0

        # make sure the value of resolution is a lowercase L,
        #  for 'low', not a numeral 1
        min_lat=np.nanmin(self.dataset[lat])-2.0
        max_lat=np.nanmax(self.dataset[lat])+2.0
        min_lon=np.nanmin(self.dataset[lon])-2.0
        max_lon=np.nanmax(self.dataset[lon])+2.0
        plt.figure()
        ax = plt.axes(projection=ccrs.Mercator())
        fa=ax.natural_earth_shp(name='land', resolution='10m', facecolor=cont_color)
        ax.coastlines(resolution='10m')
        plt.plot(self.dataset[lon][::160],self.dataset[lat][::160],transform=ccrs.Geodetic(),linewidth=trackwidth,color=trackcolor)
        ax.gridlines(draw_labels=True)
        
        strdate='/'.join([str(e) for e in self.dataset['DATE'].data])
        plt.title('Flight Track %s - %s (%s,%s)' % (self.dataset['FLIGHT'].data,strdate,lon,lat))
        gpsdata='$GPSDATA'
        gpsdata=os.path.expandvars(gpsdata) if os.path.expandvars(gpsdata)!=gpsdata else ''
        map_file=os.path.join(gpsdata,'%s_track.ps' % self.dataset['FLIGHT'].data)
        self.outputs[0].data=map_file
        plt.savefig(map_file)
        return
