from ppodd.core import *
import ppodd
#from ppodd.pod import *
import time
import os.path
from netCDF4 import Dataset
import ppodd
class write_nc(cal_base):
    """ Write data out to a NetCDF
It will try and write whatever is in input_names
beware it needs the DATE and FLIGHT parameters, and some timed data as a minimum
    """
    def __init__(self,dataset):
        self.input_names=['DATE','FLIGHT', 'Flight_Constants', 'SOL_AZIM', 'SOL_ZEN',
        'IAS_RVSM', 'TAS_RVSM', 'PA_TURB', 'PB_TURB', 'TAT_DI_R', 'PSAP_LOG',
        'P9_STAT', 'PSAP_FLO', 'PSAP_TRA', 'TAS', 'TAT_ND_R', 'CO_AERO', 'SW_DN_C', 'TDEW_GE', 'NO2_TECO',
        'CAB_TEMP', 'NOX_TECO', 'LWC_JW_U', 'TWC_DET', 'BTHEIM_U', 'TWC_TSAM',
        'P0_S10', 'AOA', 'AOSS', 'RED_DN_C', 'PSAP_LIN', 'RED_UP_C', 'CPC_CONC',
        'TWC_EVAP', 'O3_TECO', 'HGT_RADR', 'PS_RVSM', 'Q_RVSM', 'PALT_RVS', 'CAB_PRES',
        'V_C', 'U_C', 'W_C', 'V_NOTURB', 'U_NOTURB', 'PSP_TURB', 'NO_TECO', 'SO2_TECO', 'BTHEIM_C', 'TWC_TDEW',
        'NV_LWC_U', 'NV_TWC_U', 'LAT_GIN', 'LON_GIN', 'ALT_GIN', 'VELN_GIN',
        'VELE_GIN', 'VELD_GIN', 'ROLL_GIN', 'PTCH_GIN', 'HDG_GIN', 'TRCK_GIN', 'GSPD_GIN',
        'ROLR_GIN', 'PITR_GIN', 'HDGR_GIN', 'ACLF_GIN', 'ACLS_GIN', 'ACLD_GIN', 'SW_UP_C',
        'NEPH_PR', 'NEPH_T', 'TSC_BLUU', 'TSC_GRNU', 'TSC_REDU', 'BSC_BLUU', 'BSC_GRNU', 'BSC_REDU',
        'EXX_JCI', 'WOW_IND', 'WVSS2F_VMR', 'WVSS2R_VMR', 'CPC_CNTS', 'TDEW_CR2', 'TDEW_C_U']


        self.outputs=[]
        self.name='WRITE_NC'
        self.version=1.00
        cal_base.__init__(self,dataset)
        self.comment="""
FAAM data core_faam_yyyymmdd_vnnn_rn_cnnn.nc

 where yyyymmdd is the flight start date, vnnn the processing version cnnn is
 the flight number and rn the revision number.

The NetCDF file comprises a header followed by the actual data.  The header
contains a list of the parameters in the dataset - their long names, short
names, units, and measurement frequency.  For each named parameter there is a
matching parameter with FLAG added to the parameter name which contains
details of the quality of each measurement.  The FLAG parameters take four
possible values:

0 - measurement believed to be good.
Other flags need to be checked in the metadata for each measurement, which
is stored at the BADC

The NetCDF file header also contains:

1.      Flight number
2.      Flight date
3.      Data date - date datafile was created
4.      History - contains information about the data processing, not all of
        which may be relevant to the data in the final NetCDF file:
        a.      The dataset the NetCDF file was produced from
        b.      Input and output files used by the calibration process
        c.      The versions of the software modules used by the calibration
                process
        d.      Processor information
        e.      Constants used for calibration
        f.      Processing modules used for calibration
        g.      Data start and end times
        h.      Raw input parameters to the calibration process
        i.      Calibrated output parameters to the calibration process
        j.      Summary of quality flags applied by the calibration process
        k.      Details of other data added to, or taken from, the original
                calibrated dataset.
"""
    def process(self,output=None,nctype='',paras=None,onehz=False,fill_value=-9999,dtyp='f4',flag_fill=-1):
        possible_types=['NETCDF3_CLASSIC','NETCDF4', 'NETCDF4_CLASSIC', 'NETCDF3_64BIT']
        if(nctype in possible_types):
            self.netcdf_type=nctype
        else:
            self.netcdf_type='NETCDF3_CLASSIC'
        self.filename=output
        if(output and os.path.isdir(output)):
            folder=output
            self.filename=None
        else:
            folder=os.path.expandvars('$NCDATA')
        if(self.filename is None):
            try:
                self.filename=os.path.join(folder,'core_faam_%4.4i%2.2i%2.2i_' % tuple(self.dataset['DATE'][-1::-1]))
                self.filename+='%s_r%1.1i_%s' % (ppodd.version,self.dataset['revision'].data,self.dataset['FLIGHT'][:])
                if(onehz):
                    self.filename+='_1hz'
                self.filename+='.nc'
            except KeyError:
                ppodd.logger.warning("""Unknown DATE or FLIGHT to create filename
Saving as output.nc""")
                self.filename='output.nc'
        ppodd.logger.info('Creating NetCDF %s' % self.filename)
        self.coredata=Dataset(self.filename,'w',format=self.netcdf_type)
        self.coredata.comment=self.comment
        self.coredata.Data_Date=time.strftime('%Y%m%d',time.gmtime(time.time()))
        for att in self.dataset.attributes:
            ppodd.logger.info('Setting attribute %s' % str(att))
            try:
                setattr(self.coredata,att,self.dataset.attributes[att])
            except TypeError:
                try:
                    if(self.dataset.attributes[att]):
                        setattr(self.coredata,att,str(self.dataset.attributes[att]))
                except TypeError:
                    ppodd.logger.debug("Cant write this one")
        drstime = self.coredata.createDimension('Time', None)
        self.tdims={}
        paralist=[]
        times = self.coredata.createVariable('Time','i4',('Time',),fill_value=flag_fill)
        times.long_name='time of measurement'
        times.standard_name='time'
        times.calendar='gregorian'
        try:
            times.units='seconds since %4.4i-%2.2i-%2.2i 00:00:00 +0000' % tuple(self.dataset['DATE'][-1::-1])
        except KeyError:
            ppodd.logger.warning('Unknown DATE')
            times.units='seconds since midnight'
        ppodd.logging.debug('PARAS= %s' % str(paras))
        if (not paras):
            paras=self.input_names
        if paras=='all' or paras==['all']:
            paras=self.dataset.keys()
        for p in paras:
            try:
                par=self.dataset[p]
                try:
                    t=par.times
                    if(onehz):
                        dims=('Time',)
                    else:
                        f=par.frequency
                        if(f not in self.tdims):
                            name='sps%2.2i' % f
                            self.coredata.createDimension(name, f)
                            self.tdims.update({f:name})
                        dims=('Time',self.tdims[f])
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
                    except NameError:
                        end=np.max(t)
                        start=np.min(t)
                except AttributeError:
                    try:
                        ln=p.long_name
                        ppodd.logger.warning('%s has no timed data' % str(p))
                    except AttributeError:
                        # Not a data parameter so no worries
                        pass
            except KeyError:
                ppodd.logger.warning('No %s' % str(p))

        t0=time.time()
        try:
            self.coredata.TimeInterval=time.strftime('%H:%M:%S',time.gmtime(start))+'-'+time.strftime('%H:%M:%S',time.gmtime(end))
        except NameError:
            ppodd.logger.warning("No start time, probably NO DATA ! Can't write file")
            self.coredata.close()
            del self.filename
            return
        try:
            ti=timestamp((start,end))
        except (UnboundLocalError,TypeError):
            ti=None
        if ti is None:
            raise Warning('No data to write')
        else:
            length=len(ti)
            ppodd.logger.info('Writing to NetCDF:%s' % self.filename)
            times[:]=ti
            self.coredata.close()
            self.coredata=Dataset(self.filename,'a',format=self.netcdf_type)
            for p in paralist:
                par=self.dataset[p]
                para=self.coredata.variables[p]
                ppodd.logger.info('Writing %s' % par)
                f=par.frequency
                if(onehz):
                    data=par.data.get1Hz()
                else:
                    data=par.data
                try:
                    para[:]=np.float_(data).asmasked(start=start,end=end,fill_value=fill_value)
                    if hasattr(data,'flagmasked'):
                        paraf=self.coredata.variables[p+'_FLAG']
                        paraf[:]=data.flagmasked(start=start,end=end,fill_value=-1)
                except ValueError:
                    ppodd.logger.warning("Can't write %s" % par)

            self.coredata.close()
            ppodd.logger.debug('Total write time %f seconds' % (time.time()-t0))
            ppodd.logger.info('Written NetCDF:%s' % self.filename)
