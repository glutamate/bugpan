modprobe kcomedilib
insmod /usr/realtime/modules/rtai_comedi.ko
modprobe ni_pcimio
comedi_config /dev/comedi0 ni_pcimio