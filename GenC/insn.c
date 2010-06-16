/*
COPYRIGHT (C) 2009  Edoardo Vigoni   (vigoni@aero.polimi.it)
COPYRIGHT (C) 2009  Paolo Mantegazza (mantegazza@aero.polimi.it)

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
*/


#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <pthread.h>
#include <math.h>

#include <rtai_comedi.h>

#define SINGLE_INSN  0

#define SIN_FREQ  100

#define NICHAN  1
#define NOCHAN  1
#define NCHAN   (NICHAN + NOCHAN)

#if SIN_FREQ
#define FREQ   SIN_FREQ
#define N_PNT  100
#else
#define FREQ   10000
#define N_PNT  1
#endif

#define SAMP_FREQ  (FREQ * N_PNT)
#define RUN_TIME   5

#define AI_RANGE  0
#define AO_RANGE  0
#define SAMP_TIME  (1000000000/SAMP_FREQ)
static comedi_t *dev;
static int subdevai, subdevao;
static comedi_krange krangeai, krangeao;
static lsampl_t maxdatai, maxdatao;

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

	subdevao = comedi_find_subdevice_by_type(dev, COMEDI_SUBD_AO, 0);
	if (subdevao < 0) {
		comedi_close(dev);
		printf("AO subdev (6071) %d not found.\n", COMEDI_SUBD_AO);
		return 1;
	}
	comedi_get_krange(dev, subdevao, 0, AO_RANGE, &krangeao);
	maxdatao = comedi_get_maxdata(dev, subdevao, 0);
	return 0;
}

static volatile int end;
void endme(int sig) { end = 1; }

int main(void)
{
	RTIME until;
	RT_TASK *task;
	comedi_insn insn[NCHAN];
        unsigned int read_chan[NICHAN]  = { 0 };
        unsigned int write_chan[NOCHAN] = { 0 };
#if !SINGLE_INSN
	comedi_insnlist ilist = { NCHAN, insn };
#endif
#if SIN_FREQ
	lsampl_t sinewave;
	double omega = (2.0*M_PI*SIN_FREQ)/1.0E9;
	double actualtime;
#endif
	lsampl_t *hist;
	lsampl_t data[NCHAN];
	long i, k, n, retval;
	int toggle;
	FILE *fp;

	double tms[2];

#if SINGLE_INSN
	printf("single insn true\n");
#endif
#if SIN_FREQ
	printf(" true\n");
#endif
	signal(SIGKILL, endme);
	signal(SIGTERM, endme);
	hist = malloc(SAMP_FREQ*RUN_TIME*NCHAN*sizeof(lsampl_t) + 1000);
	memset(hist, 0, SAMP_FREQ*RUN_TIME*NCHAN*sizeof(lsampl_t) + 1000);

	start_rt_timer(0);
	task = rt_task_init_schmod(nam2num("MYTASK"), 1, 0, 0, SCHED_FIFO, 0xF);
	printf("COMEDI INSN%s TEST BEGINS: SAMPLING FREQ: %d, RUN TIME: %d.\n", SINGLE_INSN ? "" : "LIST", SAMP_FREQ, RUN_TIME);
	mlockall(MCL_CURRENT | MCL_FUTURE);
	rt_make_hard_real_time();

	if (init_board()) {;
		printf("Board initialization failed.\n");
		return 1;
	}

        for (i = 0; i < NICHAN; i++) {
		BUILD_AREAD_INSN(insn[i], subdevai, data[i], 1, read_chan[i], AI_RANGE, AREF_GROUND);
        }
        for (i = 0; i < NOCHAN; i++) {
		BUILD_AWRITE_INSN(insn[NICHAN + i], subdevao, data[NICHAN + i], 1, write_chan[i], AO_RANGE, AREF_GROUND);
        }
	printf("done building.\n"); fflush (stdout);
	until = rt_get_time();
	for (toggle = n = k = 0; k < SAMP_FREQ*RUN_TIME && !end; k++) {
#if SIN_FREQ
		actualtime = count2nano(rt_get_time());
		if(k<2) tms[k] = actualtime; 
		sinewave =  (int) (maxdatao/8*sin(omega*actualtime));
		data[NICHAN]     =  sinewave+maxdatao/2;
		//data[NICHAN + 1] = -sinewave+maxdatao/2;
#else
		data[NICHAN]     = toggle*maxdatao/2;
		data[NICHAN + 1] = (1 - toggle)*maxdatao/2;
		toggle = 1 - toggle;
#endif
		
#if SINGLE_INSN
		for (i = 0; i < NCHAN; i++) {
			if ((retval = comedi_do_insn(dev, insn + i)) > 0) {
				 hist[n++] = data[i];
			} else {
				printf("Comedi insn failed # %ld out of %d instructions, retval %ld.\n", i, NCHAN, retval);
				break;
			}
		}
#else
		if ((retval = rt_comedi_do_insnlist(dev, &ilist)) == NCHAN) {
			for (i = 0; i < NCHAN; i++) {
				 hist[n++] = data[i];
			}
		} else {
			printf("Comedi insnlist processed only %lu out of %d instructions.\n", retval, NCHAN);
			break;
		}
#endif
		rt_sleep_until(until += nano2count(SAMP_TIME));
	}

	comedi_cancel(dev, subdevai);
	comedi_cancel(dev, subdevao);
	comedi_data_write(dev, subdevao, 0, 0, AREF_GROUND, 2048);
	comedi_data_write(dev, subdevao, 1, 0, AREF_GROUND, 2048);
	comedi_close(dev);
	printf("COMEDI INSN%s ENDS.\n", SINGLE_INSN ? "" : "LIST");
	printf("t1: %g\n", tms[0]);
	printf("t2: %g\n", tms[1]);
	printf("tdiff: %g\n", tms[1]-tms[0]);
	fp = fopen("rec.dat", "w");
	for (n = k = 0; k < SAMP_FREQ*RUN_TIME; k++) {
		fprintf(fp, "# %ld: ", k);
		for (i = 0; i < NCHAN; i++) {
			fprintf(fp, "%d\t", hist[n++]);
		}
		fprintf(fp, "\n");
	}
	fclose(fp);
	free(hist);

	stop_rt_timer();
	rt_make_soft_real_time();
	rt_task_delete(task);

	return 0;
}
