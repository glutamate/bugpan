#include <stdio.h>   /* for printf() */
#include <stdlib.h>   /* for printf() */
#include <string.h>
#include <unistd.h>
#include <comedilib.h>

comedi_cmd *cmd = NULL;
double freq = 10000;
int aref = AREF_GROUND; /* more on this later */
comedi_t *it;
double* inp_res[32];
comedi_range* inp_rang[32];
lsampl_t inp_maxdata[32];

void internal_trigger(); 

int main() {
  

  it=comedi_open("/dev/comedi0");

  int ret;
  if(cmd==NULL) {
    cmd=malloc(sizeof(*cmd));
    if(cmd==NULL) return -1;
  } else {
    if(cmd->chanlist) free(cmd->chanlist);
  }

  comedi_set_global_oor_behavior(COMEDI_OOR_NUMBER);

  ret = prepare_cmd(COMEDI_SUBD_AI,cmd,freq);
  printf("B");fflush(stdout);
  //comedi_get_cmd_generic_timed(dev,subdevice,cmd,0,1e9/freq);
  if(ret<0){
    printf("comedi_get_cmd_generic_timed failed\n");
    return ret;
  }
  /* Modify parts of the command */
   cmd->scan_end_arg = 0; 

  cmd->stop_arg = 0;
  setup_read_wave(COMEDI_SUBD_AI, 0, 0, 100);

  ret = comedi_command(it,cmd);
    if(ret)
      printf("comedi_command (acq): %d\n", ret);

    internal_trigger();

  comedi_cancel(it, 0);
  comedi_cancel(it, 1);

}

double read_volts(int subdev, int chan, int range) {
  lsampl_t data, maxdata;
  int readres;
  comedi_range *rang;
  double outval;

  comedi_set_global_oor_behavior(COMEDI_OOR_NUMBER);
  readres=comedi_data_read(it,subdev,chan,range,aref, & data);
  if(!readres) {
    comedi_perror("comedi_data_read: ");
  }
  rang = comedi_get_range(it, subdev, chan, range);
  maxdata = comedi_get_maxdata(it, subdev, chan);
  outval = comedi_to_phys(data, rang, maxdata);
  //printf("data=%d\noutval=%g\nreadres=%d\n",data, outval, readres);
  return outval;

}

int setup_read_wave(int subd, int chan, int range, int npnts) {
  //  assert(it!=NULL);

  printf("setup_read_wave...");fflush(stdout);
  int ret;
  int wv_n=++cmd->chanlist_len, wv_i=wv_n-1;
  lsampl_t data;
  //printf("wv_i %d \n", wv_i);

  ///assert(cmd!=NULL);

  if(cmd->chanlist==NULL) {
    cmd->chanlist=malloc(sizeof(unsigned int));
  } else {
    if(realloc(cmd->chanlist, (wv_n)*sizeof(unsigned int))==NULL) 
      { printf("realloc cmd fail \n"); return -1;}
  }
  cmd->chanlist[wv_i]= CR_PACK(chan,range,aref);
  if(npnts>cmd->stop_arg) cmd->stop_arg=npnts;

  cmd->scan_end_arg=wv_n;

  //test_board_read(it);

  comedi_command_test(it,cmd);
  ret=comedi_command_test(it,cmd);
  if(ret!=0){
    comedi_perror("comedi_command_test fail on read");
    return -1;
  }
  //test_board_read(it);

  inp_res[wv_i]=malloc(npnts*sizeof(double));
  inp_rang[wv_i]=comedi_get_range(it, subd, chan, range);
  inp_maxdata[wv_i]=comedi_get_maxdata(it, subd, chan);
  comedi_data_read(it,subd,chan,range,aref, & data);
  //printf("found sdtype %d \n", comedi_find_subdevice_by_type(it, COMEDI_SUBD_AI,0));

  //printf("setup subd %d chan %d rng %d:  data=%d outval=%g\n",subd, chan, range, data, comedi_to_phys(data, inp_rang[wv_i], inp_maxdata[wv_i]));
  printf("done\n");fflush(stdout);

  return wv_i;
}




int prepare_cmd(int subdevice,comedi_cmd *cmd, double freq)
{
	memset(cmd,0,sizeof(*cmd));

	/* the subdevice that the command is sent to */
	cmd->subdev =	subdevice;

	/* flags */
	cmd->flags = 0;

	/* Wake up at the end of every scan */
	//cmd->flags |= TRIG_WAKE_EOS;
	
	/* Use a real-time interrupt, if available */
	//cmd->flags |= TRIG_RT;

	/* each event requires a trigger, which is specified
	   by a source and an argument.  For example, to specify
	   an external digital line 3 as a source, you would use
	   src=TRIG_EXT and arg=3. */

	/* The start of acquisition is controlled by start_src.
	 * TRIG_NOW:     The start_src event occurs start_arg nanoseconds
	 *               after comedi_command() is called.  Currently,
	 *               only start_arg=0 is supported.
	 * TRIG_FOLLOW:  (For an output device.)  The start_src event occurs
	 *               when data is written to the buffer.
	 * TRIG_EXT:     start event occurs when an external trigger
	 *               signal occurs, e.g., a rising edge of a digital
	 *               line.  start_arg chooses the particular digital
	 *               line.
	 * TRIG_INT:     start event occurs on a Comedi internal signal,
	 *               which is typically caused by an INSN_TRIG
	 *               instruction.
	 */

	cmd->start_src      = TRIG_INT;
	//cmd->start_arg      = 1e9;
	//cmd->start_arg      = NI_EXT_PFI(0) | CR_EDGE;


	//cmd->start_src =	TRIG_NOW;
	//cmd->start_arg =	0;

	/* The timing of the beginning of each scan is controlled by
	 * scan_begin.
	 * TRIG_TIMER:   scan_begin events occur periodically.
	 *               The time between scan_begin events is
	 *               convert_arg nanoseconds.
	 * TRIG_EXT:     scan_begin events occur when an external trigger
	 *               signal occurs, e.g., a rising edge of a digital
	 *               line.  scan_begin_arg chooses the particular digital
	 *               line.
	 * TRIG_FOLLOW:  scan_begin events occur immediately after a scan_end
	 *               event occurs.
	 * The scan_begin_arg that we use here may not be supported exactly
	 * by the device, but it will be adjusted to the nearest supported
	 * value by comedi_command_test(). */
	cmd->scan_begin_src =	TRIG_TIMER;
	cmd->scan_begin_arg =	1e9/freq;		/* in ns */

	/* The timing between each sample in a scan is controlled by convert.
	 * TRIG_TIMER:   Conversion events occur periodically.
	 *               The time between convert events is
	 *               convert_arg nanoseconds.
	 * TRIG_EXT:     Conversion events occur when an external trigger
	 *               signal occurs, e.g., a rising edge of a digital
	 *               line.  convert_arg chooses the particular digital
	 *               line.
	 * TRIG_NOW:     All conversion events in a scan occur simultaneously.
	 * Even though it is invalid, we specify 1 ns here.  It will be
	 * adjusted later to a valid value by comedi_command_test() */
	cmd->convert_src =	TRIG_TIMER;
	cmd->convert_arg =	1;		/* in ns */

	/* The end of each scan is almost always specified using
	 * TRIG_COUNT, with the argument being the same as the
	 * number of channels in the chanlist.  You could probably
	 * find a device that allows something else, but it would
	 * be strange. */
	cmd->scan_end_src =	TRIG_COUNT;
	cmd->scan_end_arg =	0;		/* number of channels */

	/* The end of acquisition is controlled by stop_src and
	 * stop_arg.
	 * TRIG_COUNT:  stop acquisition after stop_arg scans.
	 * TRIG_NONE:   continuous acquisition, until stopped using
	 *              comedi_cancel()
	 * */
	cmd->stop_src =		TRIG_COUNT;
	cmd->stop_arg =		0;

	/* the channel list determined which channels are sampled.
	   In general, chanlist_len is the same as scan_end_arg.  Most
	   boards require this.  */
	cmd->chanlist =		NULL;
	cmd->chanlist_len =	0;

	return 0;
}


void internal_trigger() {
  ///assert(cmd!=NULL);

  //printf("triggering...");fflush(stdout);
  static comedi_insn ins[2];
  static comedi_insnlist il;
  lsampl_t d1, d2;
  int ret;
  il.n_insns=1;
  il.insns=ins;
  memset(ins, 0, 2*sizeof(comedi_insn));
  d1 = 0;
  d2 = 0;

  if(cmd && cmd->chanlist_len) {
    ins[0].insn=INSN_INTTRIG;
    ins[0].subdev=0;
    ins[0].data = &d1;
    ins[0].n = 1;
  }

  ret=comedi_do_insnlist(it,&il);
  if(ret<0){
    comedi_perror("error triggering ");
  }
  //printf("done\n");fflush(stdout);

}

double readpin(int chan)
{
  comedi_t *it;
  lsampl_t data, maxdata;
  comedi_range *rang;
  int readres;
  double outval;

  if((it=comedi_open("/dev/comedi0"))==NULL) {
    printf("fail open");
  }

  comedi_set_global_oor_behavior(COMEDI_OOR_NUMBER);

  readres=comedi_data_read(it,subdev,chan,0,aref, & data);
  if(!readres) {
    comedi_perror("comedi_data_read: ");
  }
 

  rang = comedi_get_range(it, subdev, chan, 0);
  maxdata = comedi_get_maxdata(it, subdev, chan);

  outval = comedi_to_phys(data, rang, maxdata);

  printf("data=%d\noutval=%g\nreadres=%d\n",data, outval, readres);

  return outval;
}
