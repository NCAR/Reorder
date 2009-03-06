/*
 *      $Id: LidarParam.h 2 2002-07-02 14:57:33Z oye $
 *
 *      Module:          LidarParam.h
 *      Original Author: Rich Neitzel
 *      Copywrited by the National Center for Atmospheric Research
 *      Date:            $Date: 2002-07-02 08:57:33 -0600 (Tue, 02 Jul 2002) $
 *
 * revision history
 * ----------------
 * $Log$
 * Revision 1.1  2002/07/02 14:57:34  oye
 * Initial revision
 *
 * Revision 1.1  1995/08/23 15:40:19  oye
 * New.
 *
 * Revision 1.1  1995/06/16  16:32:02  thor
 * Initial revision
 *
 *
 *
 * description:
 *        
 */

#ifndef _LIDARPARAM_H
#define _LIDARPARAM_H

#ifdef __cplusplus
extern "C" {
#endif

struct lidar_parameter_data {
    char lidar_param_data[4];   /* Lidar parameter data identifier */
                                /* (four ASCII characters "LDAT"). */
    long lidar_param_data_len;  /* Length of the lidar parameter data */
                                /* block in bytes. */
    long data_sys_status;       /* Status word, bits will be assigned */
                                /* particular status when needed. */
    char lidar_name[8];         /* Name of lidar from which this data */
                                /* ray came from. Directly corresponds */
                                /* to a name given in one of the lidar */
                                /* descriptors. */
    float pulse_energy[10];     /* Pulse energy for each transmitted */
                                /* wavelength. */
    long ray_count;             /* Data ray counter for this */
                                /* particular type of data ray. */
    float derived_param[10];    /* Parameters derived form the data */
                                /* set in real time.  The definition */
                                /* and units of these parameters are */
                                /* given in the field lidar */
                                /* descriptor. */
    long event;                 /* Event counter is incremented */
                                /* everytime operator hits event */
                                /* button */
    long pmt_voltage;           /* Control voltage to the PMT power */
                                /* supply */
    long apd_voltage;           /* Control voltage to the APD power */
                                /* supply */
    long flashlmp_voltage;      /* Flash lamp power supply voltage */
    long temperatures[10];      /* Value of the monitored temperatures */
                                /* (names in the field lidar info */
                                /* block) */
    short first_rec_gate;       /* First recorded gate number (N). */
    short last_rec_gate;        /* Last recorded gate number (M) */
};

typedef struct lidar_parameter_data lidar_parameter_data;
typedef struct lidar_parameter_data LIDARDATA;

#ifdef __cplusplus
};
#endif

#endif /* _LIDARPARAM_H */
