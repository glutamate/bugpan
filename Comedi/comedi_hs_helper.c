#include <stdio.h>   /* for printf() */
#include <stdlib.h>   /* for printf() */
#include <string.h>
#include <unistd.h>
#include <comedilib.h>
#include "comedi_hs_helper.h"
//#include <assert.h>

int subdev = 0;         /* change this to your input subdevice */
//int range = 0;          /* more on this later */
int aref = AREF_GROUND; /* more on this later */
comedi_cmd *cmd = NULL;
comedi_cmd *cmd_out = NULL;
#define BUFSZ 0x8000
char buf[BUFSZ];
char buf_out[BUFSZ];
double* inp_res[32];
comedi_range* inp_rang[32];
lsampl_t inp_maxdata[32];
comedi_t *it;
unsigned int cur_pt=0;

double globalInputHz;

double* out_res[32];
comedi_range* out_rang[32];
lsampl_t out_maxdata[32];
unsigned int out_npnts;

double read_volts(comedi_t *it, int subd, int chan, int range) {
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


comedi_t* get_comedi_ptr() {
  if(it==NULL)
    if((it=comedi_open("/dev/comedi0"))==NULL) {
      printf("fail open");
    }
  return it;
  
}
void assert(int tst) {
  if(!tst) 
    printf("\n!!!!!!!!!ERROR!!!!!!!!!\n\n");
}

int subdev_type(int sdt) {
  int ret=0; 
  switch(sdt) {
  case 10: ret = COMEDI_SUBD_AI;
    break;
  case 11: ret = COMEDI_SUBD_AO;
    break;
  case 12: ret = COMEDI_SUBD_DO;
    break;
  case 13: ret = COMEDI_SUBD_DI;
    break;
  case 14: ret = COMEDI_SUBD_DIO;
    break;
  }
  return ret;
} 

int new_trial(int subdevice, double freq) {
  //from demo/cmd.c
  printf("new_trial (sdev %d)...", subdevice);fflush(stdout);

  if(it==NULL) {
    printf("O");fflush(stdout);
    if((it=comedi_open("/dev/comedi0"))==NULL) {
      perror("fail open\n");
      return -1;
    }
    }


  comedi_loglevel(4);
  test_board_read(it);

  int ret;
  if(cmd==NULL) {
    cmd=malloc(sizeof(*cmd));
    if(cmd==NULL) return -1;
  } else {
    if(cmd->chanlist) free(cmd->chanlist);
  }

  if(cmd_out) {
    if(cmd_out->chanlist) free(cmd_out->chanlist);
    free(cmd_out);
    cmd_out=NULL;
  }
  memset(cmd,0,sizeof(*cmd));

  /*int  comedi_get_cmd_generic_timed (comedi_t * device, unsigned int sub�
       device, comedi_cmd * command, unsigned int chanlist_len,  unsigned  int
       scan_period_ns);*/
  comedi_set_global_oor_behavior(COMEDI_OOR_NUMBER);

  printf("A");fflush(stdout);

  ret = prepare_cmd(subdevice,cmd,freq);
  printf("B");fflush(stdout);
  //comedi_get_cmd_generic_timed(dev,subdevice,cmd,0,1e9/freq);
  if(ret<0){
    printf("comedi_get_cmd_generic_timed failed\n");
    return ret;
  }
  /*  ret = prepare_out_cmd(dev,subdevice,freq);
  //comedi_get_cmd_generic_timed(dev,subdevice,cmd,0,1e9/freq);
  if(ret<0){
    printf("comedi_get_cmd_generic_timed failed\n");
    return ret;
    }*/

   globalInputHz = freq;

  /* Modify parts of the command */
  cmd->chanlist		= NULL;
  cmd->chanlist_len	= 0;
  
  cmd->scan_end_arg = 0; 

  cmd->stop_arg = 0;
  printf("C");fflush(stdout);

  //printf("new_trial done ok\n");
  printf("done\n");fflush(stdout);

  return 0; 
}
int new_trial_out(int subdevice, double freq) {
  //from demo/cmd.c
  int ret;

  assert(cmd_out==NULL);

  if(cmd_out==NULL) {
    cmd_out=malloc(sizeof(*cmd_out));
    if(cmd_out==NULL) return -1;
  } else {
    if(cmd_out->chanlist) free(cmd_out->chanlist);
  }
  memset(cmd_out,0,sizeof(*cmd_out));
  cur_pt = 0;
  /*int  comedi_get_cmd_generic_timed (comedi_t * device, unsigned int sub�
       device, comedi_cmd * command, unsigned int chanlist_len,  unsigned  int
       scan_period_ns);*/

     ret = prepare_out_cmd(subdevice,freq);
  //comedi_get_cmd_generic_timed(dev,subdevice,cmd,0,1e9/freq);
  if(ret<0){
    printf("comedi_get_cmd_generic_timed failed %d\n", ret);
    return ret;
    }

   globalInputHz = freq;

  /* Modify parts of the command */
  //cmd_out->chanlist		= NULL;
  //cmd_out->chanlist_len	= 0;
  
  //cmd_out->scan_end_arg = 0; 

  //cmd_out->stop_arg = 0;

  return 0; 
}

double getGlobalFreq() {
    return globalInputHz;
}
/*int setup_out_wave(comedi_t *it, int subd, int chan, int range, int npnts, double double *p) {
  if(!cmd_out)
    prepare_out_cmd(it, subd, 
}
*/



int setup_read_wave(int subd, int chan, int range, int npnts) {
  assert(it!=NULL);

  printf("setup_read_wave...");fflush(stdout);
  int ret;
  int wv_n=++cmd->chanlist_len, wv_i=wv_n-1;
  lsampl_t data;
  //printf("wv_i %d \n", wv_i);

  assert(cmd!=NULL);

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

int setup_write_wave(int subd, int chan, int range, int npnts, double *pts) {
  printf("setup_write_wave...");fflush(stdout);
  int ret;
  //int wv_n=++cmd_out->chanlist_len, wv_i=wv_n-1;
  int wv_n=1, wv_i=0;
  lsampl_t data;

  assert(cmd_out!=NULL);

  if(cmd_out->chanlist==NULL) {
    //printf("chanlist==NULL\n", ret);
    cmd_out->chanlist=malloc(sizeof(unsigned int));
  }else {
     if(realloc(cmd_out->chanlist, (wv_n)*sizeof(unsigned int))==NULL) return -1;
    }
  cmd_out->chanlist[wv_i]= CR_PACK(chan,range,aref);

  //cmd_out->scan_end_arg=wv_n;
  //printf("A out chanlist_len=%d\n", cmd_out->chanlist_len);

  comedi_command_test(it,cmd_out);
  comedi_command_test(it,cmd_out);

  ret=comedi_command_test(it,cmd_out);
  if(ret!=0){
    printf("comedi_command_test fail on write %d\n", ret);
    return -1;
  }
  //printf("B out chanlist_len=%d\n", cmd_out->chanlist_len);

  double *p = pts;
  int i;

  out_res[wv_i]=malloc(npnts*sizeof(double));

  for(i=0;i<npnts;i++) {
    out_res[wv_i][i]=*(p++);
  }

  out_npnts=npnts;
  out_rang[wv_i]=comedi_get_range(it, subd, chan, range);
  out_maxdata[wv_i]=comedi_get_maxdata(it, subd, chan);
  //comedi_data_read(it,subd,chan,range,aref, & data);
  //printf("found sdtype %d \n", comedi_find_subdevice_by_type(it, COMEDI_SUBD_AI,0));

  //printf("setup subd %d chan %d rng %d:  data=%d outval=%g\n",subd, chan, range, data, comedi_to_phys(data, inp_rang[wv_i], inp_maxdata[wv_i]));
  printf("done\n");fflush(stdout);

  return wv_i;
}

int   my_find_subdevice_by_type   (int  type, unsigned int start_subdevice) {
  
  int ret = comedi_find_subdevice_by_type(get_comedi_ptr(),type,start_subdevice);
 //printf("my_find_subdevice_by_type %d=>%d", type, ret);

 return ret;

}
int minint(int a, int b)
{
    return (a < b ? a : b);
}


int write_data() {

  assert(cmd_out!=NULL);

  if(cur_pt<out_npnts) {
    //printf("writing (cur_pt=%d)...",cur_pt );fflush(stdout);
  //lsampl_t comedi_from_phys(double data, comedi_range * range, lsampl_t maxdata);
  int i;
  sampl_t datapt;
  int n, m;
  int nsampls =BUFSZ/(sizeof(sampl_t)); 
  for(i=0;i<nsampls;i++){ //i+cur_pt<out_npnts
    if(i+cur_pt<out_npnts) {
      datapt=comedi_from_phys(out_res[0][i+cur_pt], out_rang[0], out_maxdata[0]); //+minint(i+cur_pt,out_npnts-1 )
      ((sampl_t*)buf_out)[i]=datapt;
    }  else {
      datapt=comedi_from_phys(0.1, out_rang[0], out_maxdata[0]); //+minint(i+cur_pt,out_npnts-1 )
      ((sampl_t*)buf_out)[i]=datapt;
    } 
  }
  n=BUFSZ;
  while(n>0){
    m=write(comedi_fileno(it), buf_out+(BUFSZ-n), n);
    if(m<0){
      perror("write");
      exit(0);
    }
    //printf("m=%d\n",m);
    n-=m;
  }

  cur_pt+=i;
  //printf("done, cur_pt=%d\n",cur_pt );
  return 1;
  } else {printf("no data to write, cur_pt=%d npnts=%d\n",cur_pt,out_npnts  ); return 0;}
} 


void test_board_read(comedi_t *it) {
  printf("current volts chan 0: %g\n", read_volts(it, 0, 0, 0));
 printf("current volts chan 1: %g\n", read_volts(it,0, 1, 0));
  printf("current volts chan 2: %g\n", read_volts(it,0, 2, 0));

}

void internal_trigger() {
    assert(cmd!=NULL);

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

  if(cmd_out && cmd_out->chanlist_len) {
    ins[1].insn=INSN_INTTRIG;
    ins[1].subdev=1;
    ins[1].data =&d2;
    ins[1].n = 1;
    if(cmd && cmd->chanlist_len) {
      il.n_insns=2;
    } else {
      il.insns=ins+1;
    }
  }

  ret=comedi_do_insnlist(it,&il);
  if(ret<0){
    comedi_perror("error triggering ");
  }
  //printf("done\n");fflush(stdout);

}


void prepare_cont_acq() {
  printf("prepare...");fflush(stdout);
  //test_board_read(it);

  //assert(0);
  assert(cmd!=NULL);
  int ret;
  if(cmd_out && cmd_out->chanlist) {
    ret = comedi_command(it,cmd_out);
    if(ret)
      comedi_perror("comedi_command (out) fail");
    write_data(it);
  }
  if(cmd && cmd->chanlist) {
    ret = comedi_command(it,cmd);
    if(ret)
      printf("comedi_command (acq): %d\n", ret);
  }

  if(!cmd && !cmd_out)
    printf("call prepare_cont_acq but no cmds!\n"); 
  //test_board_read(it);
  usleep(10000);
  printf("done\n");fflush(stdout);
  //internal_trigger(it);
  //start_cont_output(it);
}

void dio_set_bits(int subd,unsigned int write_mask, unsigned int bits) {
	int mybits=bits;
	comedi_dio_bitfield2(it,subd,write_mask, &mybits, 0);

}
//from demo/cmd.c

void start_cont_output() {
    assert(cmd_out!=NULL);

  int ret, i, pnt=0;
  int total=0;
  lsampl_t data;
  //printf("begin write\n");fflush(stdout);
  while(write_data(it)){} 
  //printf("done out");fflush(stdout);


}

void start_cont_acq() {
    assert(cmd!=NULL);

  int n_chan = cmd->chanlist_len;
  int ret, i, pnt=0;
  int total=0;
  lsampl_t data;
  //printf("begin acq, nchan=%d\n", n_chan);fflush(stdout);
  // see http://osdir.com/ml/comedi/2006-08/msg00003.html for trig_ext
  while(1){
    //printf("reading, no output\n");
    //if(cmd_out ==NULL) printf("cmd_out ==NULL\n");
    //else printf("cmd_out->chanlist_len=%d\n", cmd_out->chanlist_len);
    
    //printf("read\n");
    ret=read(comedi_fileno(it),buf,BUFSZ);
    //printf("done read\n");
    if(ret<0){
      /* some error occurred */
      perror("read");
      break;
    }else if(ret==0){
      /* reached stop condition */
      break;
    }else{
      static int col = 0;
      total+=ret;
      //if(verbose)fprintf(stderr,"read %d %d\n",ret,total);
      for(i=0;i<ret/4;i++){
	data=((lsampl_t*)buf)[i];
	inp_res[col][pnt]=comedi_to_phys(data, inp_rang[col], inp_maxdata[col]); // conv to phys
	//printf("%d %g",((lsampl_t *)buf)[i], inp_res[col][pnt]);
	col++;
	if(col==n_chan){
	  //printf("\n");
	  pnt++;
	  col=0;
	}
      }
    }
  } 

  //printf("done acq\n");
}

double* get_wave_ptr(int waven) {
  return inp_res[waven];
}

int get_num_stored_waves() {
  if(cmd==NULL)
    return 0;
  else return cmd->chanlist_len;
}

void free_trial_results() {
  int i;
  if(cmd) {
  for(i=0;i<cmd->chanlist_len;i++){
    free(inp_res[i]);
  } }
  //fflush(comedi_fileno(it));
  comedi_cancel(it, 0);
  comedi_cancel(it, 1);

  //printf("com_close=%d\n", comedi_close(it)); fflush(stdout);
  //it=NULL;
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

  rang = comedi_get_range(it, subdev, chan, 0);
  maxdata = comedi_get_maxdata(it, subdev, chan);

  outval = comedi_to_phys(data, rang, maxdata);

  printf("data=%d\noutval=%g\nreadres=%d\n",data, outval, readres);

  return outval;
}

/*
 * Set up a command by hand.  This will not work on some devices.
 * There is no single command that will work on all devices.
 */
int prepare_out_cmd(int subdevice, double freq) {
  int ret;
  if(cmd_out==NULL) {
    cmd_out=malloc(sizeof(*cmd_out));
    if(cmd_out==NULL) return -1;
  } else {
    if(cmd_out->chanlist) free(cmd_out->chanlist);
  }
  memset(cmd_out,0,sizeof(*cmd_out));

  //int comedi_get_cmd_generic_timed(comedi_t * device, unsigned int subdevice, comedi_cmd * command, unsigned int period_ns);
  //printf("comedi_get_cmd_generic_timed=%d\n", comedi_get_cmd_generic_timed(dev, subdevice, cmd_out, 1, 100000));
  //comedi_command_test(dev,cmd_out);
  //printf("comedi_command_test on prepare_out_cmd %d\n", comedi_command_test(dev,cmd_out));
  cmd_out->subdev =	subdevice;

  // flags 
  cmd_out->flags = CMDF_WRITE;
  cmd_out->start_src = TRIG_INT;
  cmd_out->start_arg = 0;
  //cmd_out->start_arg = NI_EXT_PFI(0) | CR_EDGE;

 cmd_out->scan_begin_src = TRIG_TIMER;
 cmd_out->scan_begin_arg = 1e9/freq;

  cmd_out->convert_src = TRIG_NOW;
  	cmd_out->convert_arg = 0;

  	cmd_out->scan_end_src = TRIG_COUNT;
  	cmd_out->scan_end_arg = 1; //n_chan;

  	cmd_out->stop_src = TRIG_NONE;
  	cmd_out->stop_arg = 0;

  
  cmd_out->chanlist = NULL;
  	cmd_out->chanlist_len = 1;
	//printf("out chanlist_len=%d\n", cmd_out->chanlist_len);
	return 0;
	
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


