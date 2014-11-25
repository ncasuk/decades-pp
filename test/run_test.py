import os
import sys

import fnmatch

root_path=os.path.join(os.path.dirname(os.path.realpath(__file__)))
data_path=os.path.join(root_path, '../data')
build_path=os.path.join(root_path, '../build')

for d in os.listdir(build_path):
    if fnmatch.fnmatch(d, 'lib*'):
        lib_path=os.path.join(build_path, d)
        print(lib_path)

sys.path.insert(0, lib_path)

from ppodd.core import *
import ppodd.pod



d=decades_dataset()
d.add_file(os.path.join(data_path, 'core_faam_20140723_r0_b863_rawdlu.zip'))
d.add_file(os.path.join(data_path, 'flight-cst_faam_20140723_r0_b863.txt'))

d.process()

#writer = write.nc
#writer.process(os.path.join(root_path,  'core_faam_20140723_v004_r999_b863.nc')
#writer.process(os.path.join(root_path,  'core_faam_20140217_v004_r999_b846_1hz.nc', onehz=True)


