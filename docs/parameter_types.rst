
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

The basis of the timed data classes. It is an ndarray with some extra functionality. 

  * Can hold a frequency value, and has methods to calculate times at different frequencies
  * Can match itself to other times
  * Can convert between different time bases

Optionally initialized from a start and end time

.. code-block:: python

    >>> t=timestamp([1507216300,1507216301,1507216302,1507216303,1507216304])
    >>> t
    timestamp(['2017-10-05T15:11:40', '2017-10-05T15:11:41', '2017-10-05T15:11:42',
       '2017-10-05T15:11:43', '2017-10-05T15:11:44'], dtype='datetime64[s]')
    >>> t2=timestamp((np.datetime64('2017-10-05T15:11:37'),np.datetime64('2017-10-05T15:11:42')))
    >>> t2
    timestamp(['2017-10-05T15:11:37', '2017-10-05T15:11:38', '2017-10-05T15:11:39',
       '2017-10-05T15:11:40', '2017-10-05T15:11:41', '2017-10-05T15:11:42'], dtype='datetime64[s]')
    >>> t.match(t2)
    timestamp(['2017-10-05T15:11:40', '2017-10-05T15:11:41', '2017-10-05T15:11:42'], dtype='datetime64[s]')
    >>> t.ismatch(t.match(t2))
    array([True,  True,  True, False, False], dtype=bool)
    >>> t.at_frequency(2)
    timestamp([['2017-10-05T15:11:40.000000000', '2017-10-05T15:11:40.500000000'],
       ['2017-10-05T15:11:41.000000000', '2017-10-05T15:11:41.500000000'],
       ['2017-10-05T15:11:42.000000000', '2017-10-05T15:11:42.500000000'],
       ['2017-10-05T15:11:43.000000000', '2017-10-05T15:11:43.500000000'],
       ['2017-10-05T15:11:44.000000000', '2017-10-05T15:11:44.500000000']], dtype='datetime64[ns]')


    timed_data
  
This attaches a timestamp to a data array so that similar time matching, frequency changes etc are possible. 

Some useful additional methods are:
  
  * ravel() which will flatten out a 2d array assuming the second dimension is frequency. 
  * timesort() which will sort the data into time order.
  * asmasked() which will return a masked array 
  * get1Hz() gets the data average per second if 2d.

.. code-block:: python

    >>> d=timed_data([[10,11],[11,12],[12,13],[13,14]],timestamp((1507216300,1507216303)))
    >>> d
    timed_data([[10, 11],
        [11, 12],
        [12, 13],
        [13, 14]])
    >>> d.frequency
    2
    >>> d.times
    timestamp(['2017-10-05T15:11:40', '2017-10-05T15:11:41', '2017-10-05T15:11:42',
       '2017-10-05T15:11:43'], dtype='datetime64[s]')
    >>> d.ravel()
    timed_data([10, 11, 11, 12, 12, 13, 13, 14])
    >>> d.ravel().times
    timestamp(['2017-10-05T15:11:40.000000000', '2017-10-05T15:11:40.500000000',
       '2017-10-05T15:11:41.000000000', '2017-10-05T15:11:41.500000000',
       '2017-10-05T15:11:42.000000000', '2017-10-05T15:11:42.500000000',
       '2017-10-05T15:11:43.000000000', '2017-10-05T15:11:43.500000000'], dtype='datetime64[ns]')
    >>> d=timed_data([1,4,5,6],timestamp([1507216300,1507216305,1507216301,1507216302]))
    >>> d
    timed_data([1, 4, 5, 6])
    >>> d.times
    timestamp(['2017-10-05T15:11:40', '2017-10-05T15:11:45', '2017-10-05T15:11:41',
       '2017-10-05T15:11:42'], dtype='datetime64[s]')
    >>> d.timesort()
    >>> d 
    timed_data([1, 5, 6, 4])
    >>> d.times 
    timestamp(['2017-10-05T15:11:40', '2017-10-05T15:11:41', '2017-10-05T15:11:42',
       '2017-10-05T15:11:45'], dtype='datetime64[s]')

    flagged_data
  
Much like timed_data, but also ties in a flag array. The flag array must be of the same size as the data array. 
