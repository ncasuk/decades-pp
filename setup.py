from distutils.core import setup
from distutils.command.build import build as distbuild

class prebuild(distbuild):
    def run(self):
        """ make fortran modules before build """
        import os,subprocess
        d=os.getcwd()
        os.chdir('ppodd/pod/fortran_modules')
        subprocess.call('make')
        os.chdir(d)
        distbuild.run(self)


setup(name = "ppodd",
    version = "001",
    description = "Post Processing of Decades Data",
    author = "Dave Tiddeman",
    author_email = "dave.tiddeman@metoffice.gov.uk",
    url = "http://www.faam.ac.uk",
    packages = ['ppodd','ppodd.gui','ppodd.pod'],
    package_data = {'ppodd.pod' : ["c_runmod.so","fortran_modules/*.for","fortran_modules/makefile"] },
    include_package_data=True,
    scripts = ["PPODD"],
    long_description = """Post-processing of decades data.""", 
    cmdclass={'build': prebuild}   
) 
