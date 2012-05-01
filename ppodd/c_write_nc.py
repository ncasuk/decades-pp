from cal_base import *
import time
from netCDF4 import Dataset
class c_write_nc(cal_base):
    def __init__(self,dataset):
        self.input_names=['DATE','FLIGHT','IR_UP_C', 'SOL_AZIM', 'SOL_ZEN', 'IAS_RVSM', 'TAS_RVSM', 'PA_TURB', 'PB_TURB', 'TAT_DI_R', 'PSAP_LOG', 'P9_STAT', 'TAS', 'TAT_ND_R', 'CO_AERO', 'SW_DN_C', 'TDEW_GE', 'NO2_TECO', 'CAB_TEMP', 'NOX_TECO', 'LWC_JW_U', 'TWC_DET', 'BTHEIM_U', 'TWC_TSAM', 'P0_S10', 'AOA', 'AOSS', 'RED_DN_C', 'PSAP_LIN', 'RED_UP_C', 'CPC_CONC', 'TWC_EVAP', 'O3_TECO', 'HGT_RADR', 'PS_RVSM', 'Q_RVSM', 'PALT_RVS', 'CAB_PRES', 'V_C', 'U_C', 'W_C', 'PSP_TURB', 'NO_TECO', 'SO2_TECO', 'BTHEIM_C', 'TWC_TDEW', 'IR_DN_C', 'NV_LWC_U', 'NV_TCW_U', 'LAT_GIN', 'LON_GIN', 'ALT_GIN', 'VELN_GIN', 'VELE_GIN', 'VELD_GIN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN', 'TRCK_GIN', 'GSPD_GIN', 'ROLR_GIN', 'PITR_GIN', 'HDGR_GIN', 'ACLF_GIN', 'ACLS_GIN', 'ACLD_GIN', 'SW_UP_C', 'NEPH_PR', 'NEPH_T', 'TSC_BLUU', 'TSC_GRNU', 'TSC_REDU', 'BSC_BLUU', 'BSC_GRNU', 'BSC_REDU']
        self.outputs=[]
        self.name='WRITE_NC'
        self.version=1.00
        cal_base.__init__(self,dataset)
    def process(self):
        try:
            self.netcdf_type=self.dataset.output_type
        except:
            self.netcdf_type='NETCDF3_CLASSIC'
        filename=None
        fill_value=-9999.0
        for fname,filetype in self.dataset.files.items():
            if(filetype=='OUTPUT'):
                filename=fname
        if(not(filename)):
            filename='core_faam_%4.4i%2.2i%2.2i_' % tuple(self.dataset.get_para('DATE')[-1::-1])
            filename+='r%1.1i_%s.nc' % (0,self.dataset.get_para('FLIGHT')[:])
        self.coredata=Dataset(filename,'w',format=self.netcdf_type)
        drstime = self.coredata.createDimension('data_point', None)
        self.tdims={}
        ti=self.dataset.get_base_time()
        length=len(ti)
        for t in sorted(self.dataset.times):
            name='sps%2.2i' % t
            self.coredata.createDimension(name, t)
            self.tdims.update({t:name})
        times = self.coredata.createVariable('Time','i4',('data_point',),fill_value=-1)
        times.long_name='time of measurement'
        times.standard_name='time'
        times.units='seconds since %4.4i-%2.2i-%2.2i 00:00:00 +0000' % tuple(self.dataset.get_para('DATE')[-1::-1])
        paras=self.dataset.para_names()
        print 'Writing to NetCDF:%s' % filename
        t0=time.time()
        paralist=[]
        for p in self.input_names:
            if p in paras:
                par=self.dataset.get_para(p)
                if(isinstance(par,parameter)):
                    f=par.frequency
                    para=self.coredata.createVariable(
                           p,'f4',('data_point',self.tdims[f]),fill_value=fill_value)
                    paraf=self.coredata.createVariable(
                           p+'_FLAG','i1',('data_point',self.tdims[f]),fill_value=-1)
                    paralist.append(p)
                    para.long_name=par.description
                    para.units=par.units
                    para.frequency=par.frequency
                    paraf.long_name='Flag for '+par.description
                    paraf.units=1
                    paraf.frequency=par.frequency
                    try:
                        para.number=par.number
                    except:
                        pass
        times[:]=ti
        self.coredata.close()
        self.coredata=Dataset(filename,'a',format=self.netcdf_type)
        for p in paralist:
            par=self.dataset.get_para(p)
            para=self.coredata.variables[p]
            paraf=self.coredata.variables[p+'_FLAG']
            print 'Writing %s' % par
            f=par.frequency
            if(f==1):
                para[:]=par.data.data[:]
                paraf[:]=par.data.flags.data
            else:
                para[:]=par.data.data.reshape(-1,f)
                paraf[:]=par.data.flags.data.reshape(-1,f)
        self.coredata.close()
        print 'Total write time %f seconds' % (time.time()-t0)
