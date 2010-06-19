#include <stdlib.h>
#include <stdio.h>
#include <math.h>


float ran0(long *);

struct signal_double {
  double t1;
  double t2;
  double dt;
  long npts;
  double *arr;
};

struct event_unit {
  double etime;
  struct event_unit *next;
};

struct event_unit* cons_event(double thetime, struct event_unit *head) {
  struct event_unit *cell = malloc(sizeof(struct event_unit));
  cell->etime = thetime;
  cell->next = head;
  return cell;
}

struct event_unit* snoc_event(double thetime, struct event_unit *tail) {
  struct event_unit *cell = malloc(sizeof(struct event_unit));
  cell->etime = thetime;
  cell->next = NULL;
  tail->next = cell;
  return cell;
}

struct signal_double* create_sig(int npnts, double dt) {
  struct signal_double* p;
  p = malloc(sizeof(struct signal_double));
  p->npts = npnts;
  p->t1=0;
  p->dt = dt;
  p->arr = malloc(npnts*sizeof(double));
  return p;
}



void free_sig(struct signal_double* sig) {
  free(sig->arr);
  free(sig);
}

void debug_sig(struct signal_double* sig) {
  int i;
  for (i=0; i<5;i++) {
    printf("%g\n", (sig->arr)[i]);
  }
}


void free_events(struct event_unit *first) {
  struct event_unit *next_head, *head = first;
  while(head) {
    next_head = head->next;
    free(head);
    head= next_head;
  }
}

double read_signal(struct signal_double *sig, double t) {
  unsigned long pnt = (t-(sig->t1))/(sig->dt);
  return sig->arr[pnt];
}

double convolution(struct signal_double *sig, struct event_unit *evs, double tnow) {
  double sum = 0;
  struct event_unit *head = evs; 
  while(head && (head->etime)<tnow) {
    sum += read_signal(sig, tnow-(head->etime));
    head=head->next;
  }
  return sum;
}


struct event_unit* forget_events(struct event_unit *head, double forget_tm) {
  while(head && (head->etime)<forget_tm) {
     head=head->next;
  }
  return head;
}

void write_word8(FILE* fp, unsigned char c) {
  fwrite(&c, 1, 1, fp);
}

void write_int(FILE *fp, long c) {
  fwrite(&c,  sizeof(double), 1, fp);
}

void write_double(FILE *fp, double c) {
  fwrite(&c, sizeof(double), 1, fp);
}

int write_signal(char *fnm, struct signal_double *sig) {
 FILE *fp;
 
  /* open the file we are writing to */
  if(!(fp = fopen(fnm, "w")))
     return 1;

  // write nsigs , type tag, t1, t2, dt, arrlength, arr.

  //we'll open this in haskell and output it in the proper format.

  //write_int(fp, 1);
  //write_int(fp, 2);
  //write_word8(fp, 8);
  //write_word8(fp, 3);
  //write_double(fp, sig->t1);
  //write_double(fp, sig->t2);
  //write_double(fp, sig->dt);
  write_int(fp, sig->npts);
  fwrite(sig->arr,sizeof(double), sig->npts,fp);

  fclose(fp);
  return 0;

}

void print_events(struct event_unit *head) {
  while(head) {
    printf("time: %g \n", head->etime);
    head = head->next;
  }

}

long count_events(struct event_unit *head) {
  long len = 0;
  while(head) {
    head = head->next;
    len++;
  }
  return len;
}

int write_events(char *fnm, struct event_unit *head) {
 FILE *fp;
 
  /* open the file we are writing to */
  if(!(fp = fopen(fnm, "w")))
     return 1;
  write_int(fp, count_events(head));

  while(head) {
    write_double(fp, head->etime);
    head = head->next;
  }
  fclose(fp);
  return 0;

}

struct event_unit *reverse_events(struct event_unit *head) {
  struct event_unit *then, *last=NULL;
  while(head) {
    then = head-> next;
    head -> next = last;
    last = head;
    head = then;
  }
  return last;

}


long seed = 0;
double ran_poisson(double rate) {
  double x = ran0(&seed);
  return -(log(1-x))/rate;
}


struct event_unit *poisson_train(double rate, double tmax) {
  struct event_unit *head = NULL;
  double tlast = 0;
  tlast=ran_poisson(rate);
  while(tlast<tmax) {
    head = cons_event(tlast, head);
    tlast+=ran_poisson(rate);
  }
  return reverse_events(head);
}



#define IA 16807
#define IM 2147483647
#define AM (1.0/IM)
#define IQ 127773
#define IR 2836
#define MASK 123459876

float ran0(long *idum)
{
        long k;
        float ans;

        *idum ^= MASK;
        k=(*idum)/IQ;
        *idum=IA*(*idum-k*IQ)-IR*k;
        if (*idum < 0) *idum += IM;
        ans=AM*(*idum);
        *idum ^= MASK;
        return ans;
}
#undef IA
#undef IM
#undef AM
#undef IQ
#undef IR
#undef MASK
/* (C) Copr. 1986-92 Numerical Recipes Software 02141.. */
