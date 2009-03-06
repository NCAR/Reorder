/*
 *	$Id: CellSpacingFP.h 2 2002-07-02 14:57:33Z oye $
 *
 *	Module:		 CellSpacing.h
 *	Original Author: Richard E. K. Neitzel
 *      Copywrited by the National Center for Atmospheric Research
 *	Date:		 $Date: 2002-07-02 08:57:33 -0600 (Tue, 02 Jul 2002) $
 *
 * revision history
 * ----------------
 * $Log$
 * Revision 1.1  2002/07/02 14:57:33  oye
 * Initial revision
 *
 * Revision 1.1  1999/04/29 17:29:04  oye
 * New descriptor "CSFD" similar to CellSpacing.h "CSPD" but has 8 entries and
 * uses floating point for cell spacing and range to first cell.
 *
 * Revision 1.2  1994/04/05  15:22:38  case
 * moved an ifdef RPC and changed else if to else and if on another line
 * >> to keep the HP compiler happy.
 * >> .
 *
 * Revision 1.3  1991/10/15  17:54:21  thor
 * Fixed to meet latest version of tape spec.
 *
 * Revision 1.2  1991/10/11  15:32:10  thor
 * Added variable for offset to first gate.
 *
 * Revision 1.1  1991/08/30  18:39:19  thor
 * Initial revision
 *
 *
 *
 * description:
 *        
 */
#ifndef INCCellSpacingFPh
#define INCCellSpacingFPh

#ifdef OK_RPC

#if defined(UNIX) && defined(sun)
#include <rpc/rpc.h>
#else 
# if defined(WRS)
#   include "rpc/rpc.h"
# endif
#endif 

#endif 

struct cell_spacing_fp_d {
    char   cell_spacing_des[4]; /* Identifier for a cell spacing descriptor */
				/* (ascii characters CSFD). */
    long   cell_spacing_des_len; /* Cell Spacing descriptor length in bytes. */
    long   num_segments;	/* Number of segments that contain cells of */
    float  distToFirst;		/* Distance to first gate in meters. */
    float  spacing[8];		/* Width of cells in each segment in m. */
    short  num_cells[8];	/* Number of cells in each segment. */
				/* equal widths. */
};				/* End of Structure */


typedef struct cell_spacing_fp_d cell_spacing_fp_d;
typedef struct cell_spacing_fp_d CELLSPACINGFP;

#ifdef OK_RPC
#if defined(sun) || defined(WRS)
bool_t xdr_cell_spacing_fp_d(XDR *, cell_spacing_fp_d *);
#endif

#endif 

#endif 









