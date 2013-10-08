"""**ppodd** has been written as the standard post processing for core data from the `FAAM <http://www.faam.ac.uk/>`_ BAe146 recorded with the `DECADES <http://www.faam.ac.uk/index.php/new-projects/196-projectspace/450-integration-decades>`_ data acquisition system, installed from October 2013. It is also compatible with the majority of data recorded with the FAAM aircraft since 2004, but not every legacy data type is supported (yet).

There is a programming interface, a command line (decades-pp), and a graphical user interface (PPODD).

It is intended to be a flexible and modular system. 

@author Dave Tiddeman
""" 
import logging
import os.path
import time
logging.basicConfig()

logger=logging.getLogger('PPODD')
logger.setLevel('DEBUG')
rouops='$ROUOPS'
rouops=os.path.expandvars(rouops) if os.path.expandvars(rouops)!=rouops else ''
flog=os.path.join(rouops,time.strftime('ppodd_log_%Y%m%d_%H%M%S.txt'))
try:
    filelog = logging.FileHandler(flog)
    filelog.setLevel(logger.level)
    logger.addHandler(filelog)
except IOError:
    logger.warning("Can't write to %s" % flog)
version='v004'

