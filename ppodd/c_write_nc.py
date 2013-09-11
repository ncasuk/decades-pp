from ppodd.core import *
import time
import os.path
from netCDF4 import Dataset
import ppodd
class c_write_nc(cal_base):
    """ Write data out to a NetCDF 
It will try and write whatever is in input_names
beware it needs the DATE and FLIGHT parameters, and some timed data as a minimum
    """
    def __init__(self,dataset):
        self.input_names=['DATE','FLIGHT','IR_UP_C', 'SOL_AZIM', 'SOL_ZEN', 
        'IAS_RVSM', 'TAS_RVSM', 'PA_TURB', 'PB_TURB', 'TAT_DI_R', 'PSAP_LOG', 
        'P9_STAT', 'TAS', 'TAT_ND_R', 'CO_AERO', 'SW_DN_C', 'TDEW_GE', 'NO2_TECO', 
        'CAB_TEMP', 'NOX_TECO', 'LWC_JW_U', 'TWC_DET', 'BTHEIM_U', 'TWC_TSAM', 
        'P0_S10', 'AOA', 'AOSS', 'RED_DN_C', 'PSAP_LIN', 'RED_UP_C', 'CPC_CONC', 
        'TWC_EVAP', 'O3_TECO', 'HGT_RADR', 'PS_RVSM', 'Q_RVSM', 'PALT_RVS', 'CAB_PRES', 
        'V_C', 'U_C', 'W_C', 'PSP_TURB', 'NO_TECO', 'SO2_TECO', 'BTHEIM_C', 'TWC_TDEW', 
        'IR_DN_C', 'NV_LWC_U', 'NV_TCW_U', 'LAT_GIN', 'LON_GIN', 'ALT_GIN', 'VELN_GIN', 
        'VELE_GIN', 'VELD_GIN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN', 'TRCK_GIN', 'GSPD_GIN', 
        'ROLR_GIN', 'PITR_GIN', 'HDGR_GIN', 'ACLF_GIN', 'ACLS_GIN', 'ACLD_GIN', 'SW_UP_C', 
        'NEPH_PR', 'NEPH_T', 'TSC_BLUU', 'TSC_GRNU', 'TSC_REDU', 'BSC_BLUU', 'BSC_GRNU', 'BSC_REDU']
        self.outputs=[]
        self.name='WRITE_NC'
        self.version=1.00
        cal_base.__init__(self,dataset)
    def process(self,oneHz=False):
        try:
            self.netcdf_type=self.dataset.output_type
        except:
            self.netcdf_type='NETCDF3_CLASSIC'
        self.filename=None
        folder=os.path.expandvars('$NCDATA')
        fill_value=-9999
        dtyp='f4'              
        flag_fill=-1
        for fname,filetype in self.dataset.files:
            if(filetype=='OUTPUT'):
                if(os.path.basename(fname)==''):
                    folder=fname
                else:
                    self.filename=fname
        if(self.filename is None):
            try:
                self.filename=os.path.join(folder,'core_faam_%4.4i%2.2i%2.2i_' % tuple(self.dataset['DATE'][-1::-1]))
                self.filename+='%s_r%1.1i_%s' % (ppodd.version,self.dataset.attributes['revision'],self.dataset['FLIGHT'][:])
                if(oneHz):
                    self.filename+='_1Hz'
                self.filename+='.nc'
            except KeyError:
                print 'Unknown DATE or FLIGHT to create self.filename'
                print 'Saving as output.nc'
                self.filename='output.nc'
        print 'Creating NetCDF %s' % self.filename
        self.coredata=Dataset(self.filename,'w',format=self.netcdf_type)
        drstime = self.coredata.createDimension('data_point', None)
        self.tdims={}
        paralist=[]
        paras=self.dataset.keys()
        times = self.coredata.createVariable('Time','i4',('data_point',),fill_value=flag_fill)
        times.long_name='time of measurement'
        times.standard_name='time'
        try:
            times.units='seconds since %4.4i-%2.2i-%2.2i 00:00:00 +0000' % tuple(self.dataset['DATE'][-1::-1])
        except KeyError:
            print 'Unknown DATE'
            times.units='seconds since midnight'
        for p in self.input_names:
            try:
                par=self.dataset[p]
                try:                    
                    print p,
                    t=par.times
                    if(oneHz):
                        dims=('data_point',)
                    else:
                        f=par.frequency
                        if(f not in self.tdims):
                            name='sps%2.2i' % f
                            self.coredata.createDimension(name, f)
                            self.tdims.update({f:name})
                        dims=('data_point',self.tdims[f])
                    para=self.coredata.createVariable(
                       p,dtyp,dims,fill_value=fill_value)
                    paralist.append(p)
                    for att in par.attributes:
                        setattr(para,att,par.attributes[att])
                    if hasattr(par.data,'flagmasked'):
                        paraf=self.coredata.createVariable(
                               p+'_FLAG','i1',dims,fill_value=flag_fill)
                        for att in par.attributes:
                            if att not in ['units','number','standard_name']:
                                setattr(paraf,att,par.attributes[att])
                        paraf.long_name='Flag for '+par.long_name                    
                    try:
                        para.number=par.number
                    except:
                        pass
                    try:
                        end=max(end,np.max(t))
                        start=min(start,np.min(t))
                        print (start,end)
                    except NameError:
                        end=np.max(t)
                        start=np.min(t)
                        print (start,end)
                except AttributeError as ae:
                    # This is probably a constants_parameter with no times
                    print p,' has no timed data, probably a constant'
            except KeyError:
                print 'No',p       
                             
        t0=time.time()
        if self.dataset.starttime is not None:
            start=self.dataset.starttime
        if self.dataset.endtime is not None:
            end=self.dataset.endtime
        try:
            self.dataset.timeinterval=time.strftime('%H:%M:%S',time.gmtime(start))+'-'+time.strftime('%H:%M:%S',time.gmtime(end))
            self.dataset.data_date=time.strftime('%Y%m%d',time.gmtime(time.time()))
            self.dataset.processing_version=ppodd.version
        except NameError:
            print "No start time, probably NO DATA !"
            print "Can't write file"
            self.coredata.close()
            del self.filename
            return
        print start,end
        try:
            ti=timestamp((start,end))
            print 'Start %f, end %f' % (start,end)
        except (UnboundLocalError,TypeError):
            ti=None
        if ti is None:
            raise Warning('No data to write')
        else:
            length=len(ti)
            print 'Writing to NetCDF:%s' % self.filename
            times[:]=ti
            self.coredata.close()
            self.coredata=Dataset(self.filename,'a',format=self.netcdf_type)
            for p in paralist:
                par=self.dataset[p]
                para=self.coredata.variables[p]
                print 'Writing %s' % par
                f=par.frequency
                if(oneHz):
                    data=par.data.get1Hz()
                else:
                    data=par.data
                try:
                    para[:]=np.float_(data).asmasked(start=start,end=end,fill_value=fill_value)
                    if hasattr(data,'flagmasked'):
                        paraf=self.coredata.variables[p+'_FLAG']
                        paraf[:]=data.flagmasked(start=start,end=end,fill_value=-1)
                except ValueError:
                    print "Can't write %s" % par
            for att in self.dataset.attributes:
                print 'Setting attribute ',att
                print 'Value = ',self.dataset.attributes[att]
                print 'Type = ',type(self.dataset.attributes[att])
                try:
                    setattr(self.coredata,att,self.dataset.attributes[att])
                except TypeError:
                    try:
                        if(self.dataset.attributes[att]):
                            setattr(self.coredata,att,str(self.dataset.attributes[att]))
                    except TypeError:
                        print "Cant write this one"
                
            self.coredata.close()
            print 'Total write time %f seconds' % (time.time()-t0)
            print 'Written NetCDF:%s' % self.filename

