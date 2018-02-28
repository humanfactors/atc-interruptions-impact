#!/bin/bash
convert +append exp1.deferredHandoffMiss.png exp1.hrt.png ./joined/exp1-dh.png
convert +append exp1.reserror.png exp1.reslag.png ./joined/exp1-dc.png

convert +append exp2.deferredHandoffMiss.png exp2.hrt.png ./joined/exp2-dh.png
convert +append exp2.reserror.png exp2.reslag.png ./joined/exp2-dc.png

