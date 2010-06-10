#define IA 16807
#define IM 2147483647
#define AM (1.0/IM)
#define IQ 127773
#define IR 2836
#define MASK 123459876

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

void free_events(struct event_unit *first) {
  struct event_unit *next_head, *head = first;
  while(head) {
    next_head = head->next;
    free(head);
    head= next_head;
  }
}

double read_signal(struct signal_double *sig, double t) {
  unsigned long pnt = (t-sig->t1)/dt;
  return sig->arr[pnt];
}

double convolution(struct signal_double *sig, struct event_unit *evs, double forget, double tnow) {
  double sum = 0;
  struct event_unit *head = evs; 
  while(head && (head->etime)<tnow) {
    sum += read_signal(sig, tnow-head->etime);
    head=head->next;
  }
  return sum
}

void write_word8(FILE *fp, unsigned char c) {
  fwrite(&c, 1, 1, fp);
}

void write_int(FILE *fp, long c) {
  fwrite(&c,  sizeof(double), 1, fp);
}

void write_double(FILE *fp, double c) {
  fwrite(&c, sizeof(double), 1, fp);
}

void write_signal(char *fnm, struct signal_double *sig) {
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

}

struct event_unit *poisson_train(double rate, double tmax) {
  struct event_unit *head = NULL;
  double tlast = 0;
  tlast=poisson(rate);
  while(tlast<tmax) {
    head = cons_event(tlast, head);
    tlast+=poisson(rate);
  }
  return head;
}

double ran_poisson(double rate) {
  double x = ran0(seed);
  return -(log(1-x))/rate;
}

long seed = 0;

float ran0(long &idum)
// "Minimal" random number generator of Park and Miller with
// Bays-Durham shuffle and added safeguards. Returns a uniform random
// deviate between 0.0 and 1.0. Set or reset idum to any integer value
// (except the unlikely value MASK) to initialize the sequence; idum
// must not be altered between calls for successive deviates in a sequence.
{
  long k;
  float ans;

  idum ^= MASK;                   // XORing with MASK allows use of 0 and
  k = idum/IQ;                    //     other simple bit patterns for idum.
  idum = IA * (idum-k*IQ) - IR*k; // Compute idum = (IA*idum) % IM without
  if (idum < 0) idum += IM;       //     overflows by Schrage's method.
  ans = AM * idum;                // Convert idum to a floating result.
  idum ^= MASK;                   // Unmask before return.
  return ans;
}

