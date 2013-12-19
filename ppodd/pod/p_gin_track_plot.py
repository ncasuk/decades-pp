import ppodd
from ppodd.core import *
import os.path
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import numpy as np

class gin_track_plot(cal_base):
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

        from mpl_toolkits.basemap import Basemap
        import matplotlib.pyplot as plt
        import numpy as np
         
        # make sure the value of resolution is a lowercase L,
        #  for 'low', not a numeral 1
        min_lat=np.nanmin(self.dataset[lat])-2.0
        max_lat=np.nanmax(self.dataset[lat])+2.0
        min_lon=np.nanmin(self.dataset[lon])-2.0
        max_lon=np.nanmax(self.dataset[lon])+2.0
        lat_0=(min_lat+max_lat)/2.0
        lon_0=(min_lon+max_lon)/2.0
        m = Basemap(projection='merc', lat_0=lat_0, lon_0=lon_0,
            resolution = 'h', area_thresh = 0.1,
            llcrnrlon=min_lon, llcrnrlat=min_lat,
            urcrnrlon=max_lon, urcrnrlat=max_lat)
         
        m.drawcoastlines()
        m.drawcountries()
        m.fillcontinents(color=cont_color)
        m.drawmapboundary()
         
        m.drawmeridians(np.arange(0, 360, 2))
        m.drawparallels(np.arange(-90, 90, 2))
        x,y=m(self.dataset[lon].ravel(),self.dataset[lat].ravel())
        plt.plot(x,y,color=trackcolor,linewidth=trackwidth)
        strdate='/'.join([str(e) for e in self.dataset['DATE'].data])
        plt.title('Flight Track %s - %s (%s,%s)' % (self.dataset['FLIGHT'].data,strdate,lon,lat))
        gpsdata='$GPSDATA'
        gpsdata=os.path.expandvars(gpsdata) if os.path.expandvars(gpsdata)!=gpsdata else ''
        map_file=os.path.join(gpsdata,'%s_track.ps' % self.dataset['FLIGHT'].data)        
        self.outputs[0].data=map_file
        plt.savefig(map_file)
        return 
   
