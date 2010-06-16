#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <pthread.h>

#include <rtai_comedi.h>

#define NICHAN  1
#define NOCHAN  1
#define NCHAN   (NICHAN + NOCHAN)

#define AI_RANGE  0
#define AO_RANGE  0

static comedi_t *dev;
static int subdevai, subdevao;
static comedi_krange krangeai, krangeao;
static comedi_range *rangeai, *rangeao;
static lsampl_t maxdatai, maxdatao;

double to_phys(lsampl_t d) {
  return comedi_to_phys(d, rangeao, maxdataao);
}

lsampl_t from_phys(double x) {
  return comedi_from_phys(x, rangeai, maxdataai);
}

static int init_board(void)
{
	dev = comedi_open("/dev/comedi0");		
	printf("Comedi device (6071) handle: %p.\n", dev);
	if (!dev){
		printf("Unable to open (6071) %s.\n", "/dev/comedi0");
		return 1;
	}

	subdevai = comedi_find_subdevice_by_type(dev, COMEDI_SUBD_AI, 0);
	if (subdevai < 0) {
		comedi_close(dev);
		printf("AI subdev (6071) %d not found.\n", COMEDI_SUBD_AI);
		return 1;
	}
	comedi_get_krange(dev, subdevai, 0, AI_RANGE, &krangeai);
	maxdatai = comedi_get_maxdata(dev, subdevai, 0);
	rangeai=comedi_get_range(dev, subdevai, 0, 0);

	subdevao = comedi_find_subdevice_by_type(dev, COMEDI_SUBD_AO, 0);
	if (subdevao < 0) {
		comedi_close(dev);
		printf("AO subdev (6071) %d not found.\n", COMEDI_SUBD_AO);
		return 1;
	}
	comedi_get_krange(dev, subdevao, 0, AO_RANGE, &krangeao);
	maxdatao = comedi_get_maxdata(dev, subdevao, 0);
	rangeao=comedi_get_range(dev, subdevao, 0, 0);
	return 0;
}


static volatile int end;
void endme(int sig) { end = 1; }


