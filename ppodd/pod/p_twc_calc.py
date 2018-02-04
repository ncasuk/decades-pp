import numpy as np
from ppodd.core import cal_base, flagged_data, parameter
from ppodd.humidity_formulae import (vp2vmr, vmr_mmr, vp2dp)


class twc_calc(cal_base):
    """
    Use the fit to create a Mass Mixing ratio for the TWC and then a dewpoint.
    """

    def __init__(self, dataset):
        """
        Object initialization.

        Args:
            dataset  (a ppodd.core.decades_dataset): the dataset to process.
        """

        self.fit = 'TWC_FIT_GE'
        self.input_names = [self.fit, 'PS_RVSM', 'TWC_DET', 'TWC_TSAM']
        self.outputs = [
            parameter(
                'TWC_TDEW', units='K', number=725,
                long_name='Dew-point derived from TWC probe specific humidity (valid in cloud-free air)',
                standard_name='dew_point_temperature'
            ),
            parameter(
                'TWC_EVAP', units='gram kg-1', number=572,
                long_name='Total water specific humidity from the TWC evaporator instrument',
                standard_name='mass_concentration_of_water_vapor_in_air'
            )
        ]

        self.version = 1.00
        cal_base.__init__(self, dataset)

    def __str__(self):
        """Implement str()."""
        return 'Apply TWC fit ' + self.fit

    def process(self):
        """Entry hook for the processing job."""

        d = self.dataset
        self.outputs[0].frequency = d['TWC_DET'].frequency
        self.outputs[1].frequency = d['TWC_DET'].frequency

        tfull = d['TWC_DET'].data.copy()
        tfullx = tfull.times2d.ravel()

        sh = tfull.shape
        vf = tfull.flag

        F = 0.93
        Kv = 427.0
        p0 = 1013.2
        uO = 0.2095

        if len(d[self.fit].data) == 2:

            fit = np.array(d[self.fit].data)
            print('Applying FIT={}'.format(fit))

            ans = np.polyval(fit, tfull)

            px = d['PS_RVSM'].data.ravel()
            p1 = px.interp(times=tfullx).reshape(sh)

            tx = d['TWC_TSAM'].data.ravel()
            t2 = tx.interp(times=tfullx).reshape(sh)

            KO = 0.304 + 0.351 * p1 * F / p0
            vpo = (ans - (KO * uO * p1 / (Kv * t2))) * t2
            vmro = vp2vmr(vpo, p1)
            mmr = vmr_mmr(vmro)
            dp = vp2dp(vpo.ravel()).reshape(sh)

        else:
            dp = np.zeros(sh)
            mmr = tfull
            vf[:] = 3

        self.outputs[0].data = flagged_data(dp, tfull.times, vf)
        self.outputs[1].data = flagged_data(mmr, tfull.times, vf)
