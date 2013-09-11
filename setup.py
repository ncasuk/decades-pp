from distutils.core import setup
from distutils.command.build import build as distbuild

class prebuild(distbuild):
    def run(self):
        """ make fortran modules before build """
        import os,subprocess
        d=os.getcwd()
        os.chdir('ppodd/fortran_modules')
        subprocess.call('make')
        os.chdir(d)
        distbuild.run(self)


setup(name = "ppodd",
    version = "001",
    description = "Post Processing of Decades Data",
    author = "Dave Tiddeman",
    author_email = "dave.tiddeman@metoffice.gov.uk",
    url = "http://www.faam.ac.uk",
    packages = ['ppodd','ppodd.gui'],
    package_data = {'ppodd' : ["MFDPARDESC.DAT","c_runmod.so"] },
    include_package_data=True,
    scripts = ["processdd","PPODD"],
    long_description = """Post-processing of decades data.""", 
    cmdclass={'build': prebuild}   
) 
