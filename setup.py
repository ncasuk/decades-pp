from distutils.core import setup
from distutils.command.build import build as distbuild

class fortbuild(distbuild):
    def run(self):
        """ make fortran modules after build """
        distbuild.run(self)
        import os,subprocess
        d=os.getcwd()
        print self.build_lib
        os.chdir(os.path.join(self.build_lib,'ppodd/pod/fortran_modules'))
        subprocess.call('make')
        os.chdir(d)


setup(name = "ppodd",
    version = "002",
    description = "Post Processing of Decades Data",
    author = "Dave Tiddeman",
    author_email = "dave.tiddeman@metoffice.gov.uk",
    url = "http://www.faam.ac.uk",
    packages = ['ppodd','ppodd.gui','ppodd.pod'],
    package_data = {'ppodd.pod' : ["runmod.so","fortran_modules/*.for","fortran_modules/makefile"] },
    include_package_data=True,
    scripts = ["PPODD"],
    long_description = """Post-processing of decades data."""
    ,cmdclass={'build': fortbuild}   
) 
