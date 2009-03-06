#ifndef PIRAQX_H
#define PIRAQX_H
/**
 * @file   piraqx.h
 
 * $Date: 2003-10-09 12:37:01 -0600 (Thu, 09 Oct 2003) $
 * $Id: piraqx.h 187 2003-10-09 18:37:01Z oye $

 *
 * 
 *  @author Joseph VanAndel <vanandel@ucar.edu>
 */
#include "dd_types.h"
#include "dd_defines.h"

/* definition of several different data formats */
#ifndef DATA_SIMPLEPP

#define DATA_SIMPLEPP    0 /* simple pulse pair ABP */
#define DATA_POLYPP      1 /* poly pulse pair ABPAB */
#define DATA_POL1        3 /* dual polarization pulse pair ABP,ABP */
#define DATA_POL2        4 /* more complex dual polarization ??????? */
#define DATA_POL3        5 /* almost full dual polarization with log integers */
#define DATA_SIMPLEPP16  6 /* simple pulse pair ABP (16-bit ints not floats) */
#define DATA_DOW        7 /* dow data format */
#define DATA_POL12       8 /* simple pulse pair ABP (16-bit ints not floats) */
#define DATA_POL_PLUS    9 /* full pol plus */
#define DATA_MAX_POL    10 /* same as full plus plus more gates */
#define DATA_HVSIMUL    11 /* simultaneous transmission of H and V */
#define DATA_SHRTPUL    12 /* same as MAX_POL with gate averaging */
#define DATA_SMHVSIM    13 /* 2000 DOW4 copolar matrix for simultaneous H-V (no iq average) */
#define DATA_DUALPP     15 /* DOW dual prt pulse pair ABP,ABP */
/* ABP data computed in piraq3: rapidDOW * project */
#define	PIRAQ_ABPDATA   16

/* Staggered PRT ABP data computed  in piraq3: rapidDOW project */
#define    PIRAQ_ABPDATA_STAGGER_PRT 17


#define DATA_TYPE_MAX PIRAQ_ABPDATA_STAGGER_PRT /* limit of data types */ 

#define DATA_POL_PLUS_CMP 29	/* full pol plus */
#define DATA_MAX_POL_CMP  30	/* same as full plus plus more gates */
#define DATA_HVSIMUL_CMP  31	/* simultaneous transmission of H and V */
#define DATA_SHRTPUL_CMP  32	/* same as MAX_POL with gate averaging */

#define PIRAQX_CURRENT_REVISION 1


#define PIRAQ3_MAX_GATES 2000
#define PIRAQ3_MAX_TS_GATES 20  /* maximum # of time-series gates */
#define PIRAQ3_MAX_TS_SAMPLES 256 /* maximum # of time-series samples/gate */

#endif /* DATA_SIMPLEPP */



struct piraqX_header_rev1
{		/* /code/oye/solo/translate/piraq.h
         * all elements start on 4-byte boundaries
         * 8-byte elements start on 8-byte boundaries
         * character arrays that are a multiple of 4
         * are welcome
         */
    char desc[4];			/* "DWLX" */
    uint4 recordlen;        /* total length of record - must be the second field */
    uint4 channel;          /* e.g., RapidDOW range 0-5 */
    uint4 rev;		        /* format revision #-from RADAR structure */
    uint4 one;			    /* always set to the value 1 (endian flag) */
    uint4 byte_offset_to_data;
    uint4 dataformat;

    uint4 typeof_compression;	/*  */
/*
      Pulsenumber (pulse_num) is the number of transmitted pulses
since Jan 1970. It is a 64 bit number. It is assumed
that the first pulse (pulsenumber = 0) falls exactly
at the midnight Jan 1,1970 epoch. To get unix time,
multiply by the PRT. The PRT is a rational number a/b.
More specifically N/Fc where Fc is the counter clock (PIRAQ_CLOCK_FREQ),
and N is the divider number. So you can get unix time
without roundoff error by:
secs = pulsenumber * N / Fc. The
computations is done with 64 bit arithmatic. No
rollover will occur.

The 'nanosecs' field is derived without roundoff
error by: 100 * (pulsenumber * N % Fc).

Beamnumber is the number of beams since Jan 1,1970.
The first beam (beamnumber = 0) was completed exactly
at the epoch. beamnumber = pulsenumber / hits. 
*/
    

#ifdef _TMS320C6X   /* TI doesn't support long long */
    uint4 pulse_num_low;
    uint4 pulse_num_high;
#else
    uint8 pulse_num;   /*  keep this field on an 8 byte boundary */
#endif
#ifdef _TMS320C6X   /* TI doesn't support long long */
    uint4 beam_num_low;
    uint4 beam_num_high;
#else
    uint8 beam_num;	/*  keep this field on an 8 byte boundary */
#endif
    uint4 gates;
    uint4 start_gate;
    uint4 hits;
/* additional fields: simplify current integration */
    uint4 ctrlflags; /* equivalent to packetflag below?  */
    uint4 bytespergate; 
    float4 rcvr_pulsewidth;
#define PX_NUM_PRT 4
    float4 prt[PX_NUM_PRT];
    float4 meters_to_first_gate;  

    uint4 num_segments;  /* how many segments are we using */
#define PX_MAX_SEGMENTS 8
    float4 gate_spacing_meters[PX_MAX_SEGMENTS];
    uint4 gates_in_segment[PX_MAX_SEGMENTS]; /* how many gates in this segment */
    
    

#define PX_NUM_CLUTTER_REGIONS 4
    uint4 clutter_start[PX_NUM_CLUTTER_REGIONS]; /* start gate of clutter filtered region */
    uint4 clutter_end[PX_NUM_CLUTTER_REGIONS];  /* end gate of clutter filtered region */
    uint4 clutter_type[PX_NUM_CLUTTER_REGIONS]; /* type of clutter filtering applied */

#define PIRAQ_CLOCK_FREQ 10000000  /* 10 Mhz */

/* following fields are computed from pulse_num by host */
    uint4 secs;     /* Unix standard - seconds since 1/1/1970
                       = pulse_num * N / ClockFrequency */
    uint4 nanosecs;  /* within this second */
    float4 az;   /* azimuth: referenced to 9550 MHz. possibily modified to be relative to true North. */
    float4 az_off_ref;   /* azimuth offset off reference */ 
    float4 el;		/* elevation: referenced to 9550 MHz.  */ 
    float4 el_off_ref;   /* elevation offset off reference */ 

    float4 radar_longitude;
    float4 radar_latitude;
    float4 radar_altitude;
#define PX_MAX_GPS_DATUM 8
    char gps_datum[PX_MAX_GPS_DATUM]; /* e.g. "NAD27" */
    
    uint4 ts_start_gate;   /* starting time series gate , set to 0 for none */
    uint4 ts_end_gate;     /* ending time series gate , set to 0 for none */
    
    float4 ew_velocity;

    float4 ns_velocity;
    float4 vert_velocity;

    float4 fxd_angle;		/* in degrees instead of counts */
    float4 true_scan_rate;	/* degrees/second */
    uint4 scan_type;
    uint4 scan_num;
    uint4 vol_num;

    uint4 transition;
    float4 xmit_power;

    float4 yaw;
    float4 pitch;
    float4 roll;
    float4 track;
    float4 gate0mag;  /* magnetron sample amplitude */
    float4 dacv;
    uint4  packetflag; 

    /*
    // items from the depricated radar "RHDR" header
    // do not set "radar->recordlen"
    */

    uint4 year;             /* e.g. 2003 */
    uint4 julian_day;
    
#define PX_MAX_RADAR_NAME 16
    char radar_name[PX_MAX_RADAR_NAME];
#define PX_MAX_CHANNEL_NAME 16
    char channel_name[PX_MAX_CHANNEL_NAME];
#define PX_MAX_PROJECT_NAME 16
    char project_name[PX_MAX_PROJECT_NAME];
#define PX_MAX_OPERATOR_NAME 12
    char operator_name[PX_MAX_OPERATOR_NAME];
#define PX_MAX_SITE_NAME 12
    char site_name[PX_MAX_SITE_NAME];
    

    uint4 polarization;
    float4 test_pulse_pwr;
    float4 test_pulse_frq;
    float4 frequency;

    float4 noise_figure;
    float4 noise_power;
    float4 receiver_gain;
    float4 E_plane_angle;  /* offsets from normal pointing angle */
    float4 H_plane_angle;
    

    float4 data_sys_sat;
    float4 antenna_gain;
    float4 H_beam_width;
    float4 V_beam_width;

    float4 xmit_pulsewidth;
    float4 rconst;
    float4 phaseoffset;

    float4 zdr_fudge_factor;

    float4 mismatch_loss;
    float4 rcvr_const;

    float4 test_pulse_rngs_km[2];
    float4 antenna_rotation_angle;   /* S-Pol 2nd frequency antenna may be 30 degrees off vertical */
    
#define PX_SZ_COMMENT 64
    char comment[PX_SZ_COMMENT];
    float4 i_norm;  /* normalization for timeseries */
    float4 q_norm;
    float4 i_compand;  /* companding (compression) parameters */
    float4 q_compand;
    float4 transform_matrix[2][2][2];
    float4 stokes[4]; 
    
# ifdef obsolete
    float4 spare[20];
# else
    float4 vxmit_power;
    float4 vtest_pulse_pwr;
    float4 vnoise_power;
    float4 vreceiver_gain;
    float4 vantenna_gain;
    float4 h_rconst;
    float4 v_rconst;
    float4 peak_power;            /* added by JVA -  needed for
                                     v/h_channel_radar_const */
    float4 spare[12];
# endif

    /*
    // always append new items so the alignment of legacy variables
    // won't change
    */
};

typedef struct piraqX_header_rev1 PIRAQX;

/* to ease the transition for legacy DRX code */
typedef struct piraqX_header_rev1 INFOHEADER;

#endif /* PIRAQX_H */
