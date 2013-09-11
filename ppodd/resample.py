import numpy as np
def createtimes(times1):
    """Create 1Hz times with gaps for gaps in input times of more than 0.5 sec""" 
    i=times1[1:]-times1[:-1]
    jumps=np.where(i>0.5)[0]
    jumps=np.append(jumps,len(times1)-1)
    j0=0
    t2=np.array([])
    for j in jumps:
        try:
            if(np.floor(times1[j0])>t2[-1]):
                t2=np.append(t2,np.arange(np.floor(times1[j0]),np.ceil(times1[j])))
            else:
                t2=np.append(t2,np.arange(np.ceil(times1[j0]),np.ceil(times1[j])))
        except IndexError:
            t2=np.arange(np.floor(times1[j0]),np.ceil(times1[j]))
        j0=j+1
    return t2


