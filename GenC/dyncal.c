#include <comedilib.h>
#include <math.h>
#include "dyncal.h"

comedi_t *devc;

comedi_calibration_t* comcal;
comedi_polynomial_t conv_ai;
comedi_polynomial_t conv_ao;

int cal() {

	int subdevai, subdevao;

  printf("calibrating.."); fflush(stdout);
  	devc = comedi_open("/dev/comedi0");		
	if (!devc){
		printf("Unable to open (6071) %s.\n", "/dev/comedi0");
		return 1;
	}      
	subdevai = comedi_find_subdevice_by_type(devc, COMEDI_SUBD_AI, 0);
	subdevao = comedi_find_subdevice_by_type(devc, COMEDI_SUBD_AO, 0);
	comcal = comedi_parse_calibration_file("/usr/var/lib/comedi/calibrations/ni_pcimio_pci-6289_comedi0");
	comedi_get_softcal_converter(subdevai,0, 0, COMEDI_TO_PHYSICAL, comcal, &conv_ai);
	comedi_get_softcal_converter(subdevao,0, 0, COMEDI_FROM_PHYSICAL, comcal, &conv_ao);



	//comedi_cleanup_calibration_file(cal);
	comedi_close(devc);
  printf("done\n");fflush(stdout);

  return 0;
}


double to_phys(lsampl_t d) {
  return comedi_to_physical(d, &conv_ai);
  //return 20.0*((double)d)/((double) 100)-10.0;//comedi_to_phys(d, rangeao, maxdataao);
}

lsampl_t from_phys(double x) {
  return comedi_from_physical(x, &conv_ao);
  //return llround((x+10)*((double) 100)/20); //comedi_from_phys(x, rangeai, maxdataai);
}
