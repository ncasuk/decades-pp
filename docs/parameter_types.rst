
===============
Parameter types
===============

There are three types of parameters defined, all a parameter needs as a minimum is a name, some data, and a type. This is all the constants_parameter has. What it then defines is that getting items or slices from the parameter is equivalent to doing the same from its data attribute.

The file parameter is much the same, but defines the data as a set (of filenames). 

The parameter or data parameter has all the common attributes of a NetCDF variable. The data however is of the type timed_data so that time matching can be done before calculations.

==========
Data types
==========

There are three main data types defined. They may in the future, be rewritten using some parts of the standard pandas library, as some of the functionality is similar. 

timestamp
=========

The basis of the timed data classes. It is an ndarray with some extra functionality. 
* Can hold a frequency value, and has methods to calculate times at different frequencies
* Can match itself to other times
* Can convert between different time bases

Optionally initialized from a start and end time

Examples::

    >>> t=timestamp([10,12,13,14,16])
    >>> t
    timestamp([ 10., 12., 13., 14., 16.])
    >>> t2=timestamp((12,18))
    >>> t2
    timestamp([ 12., 13., 14., 15., 16., 17., 18.])
    >>> t.match(t2)
    timestamp([ 12., 13., 14., 16.])
    >>> t.ismatch(t.match(t2))
    array([False, True, True, True, True], dtype=bool)
    >>> t.at_frequency(2)
    timestamp([[ 10. , 10.5],
        [ 12. , 12.5],
        [ 13. , 13.5],
        [ 14. , 14.5],
        [ 16. , 16.5]])
    timed_data
  
This attaches a timestamp to a data array so that similar time matching, frequency changes etc are possible. 

Some useful additional methods are:
* ravel() which will flatten out a 2d array assuming the second dimension is frequency. 
* timesort() which will sort the data into time order.
* asmasked() which will return a masked array 
* get1Hz() gets the data average per second if 2d.


Examples::

    >>> d=timed_data([[10,11],[11,12],[12,13],[13,14]],timestamp((36000,36003)))
    >>> d
    timed_data([[10, 11],
        [11, 12],
        [12, 13],
        [13, 14]])
    >>> d.frequency
    2
    >>> d.times
    timestamp([ 36000., 36001., 36002., 36003.])
    >>> d.ravel()
    timed_data([10, 11, 11, 12, 12, 13, 13, 14])
    >>> d.ravel().times
    timestamp([ 36000. , 36000.5, 36001. , 36001.5, 36002. , 36002.5,   36003. , 36003.5])
    >>> d=timed_data([1,4,5,6],timestamp([36000,36005,36001,36002]))
    >>> d
    timed_data([1, 4, 5, 6])
    >>> d.times
    timestamp([ 36000., 36005., 36001., 36002.])
    >>> d.timesort()
    >>> d 
    timed_data([1, 5, 6, 4])
    >>> d.times 
    timestamp([ 36000., 36001., 36002., 36005.])
    flagged_data
  
Much like timed_data, but also ties in a flag array. The flag array must be of the same size as the data array. 
