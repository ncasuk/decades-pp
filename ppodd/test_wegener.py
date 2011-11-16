import Ppodd as Ppodd
d=Ppodd.decades_dataset()
d.add_file('data/rawdata/b638_raw','M3')
d.add_file('data/fltcons/flight-cst_faam_20110902_r1_b638.txt','CONST')
d.add_file('data/gpsdata/b638_gin.dat','GIN')
d.add_file('Ppodd/b638_data.nc','OUTPUT_NETCDF3_CLASSIC')
d.files
callist=Ppodd.calibrate(d)


