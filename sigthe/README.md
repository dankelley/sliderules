Concept
=======

In a linear approximation to the seawater equation of state, assuming
constant pressure *p*, potential density anomaly *σ*<sub>*θ*</sub> is
given by where
*α* =  − *σ*<sub>*θ*</sub><sup> − 1</sup>∂*σ*<sub>*θ*</sub>/∂*T* and
*β* = *σ*<sub>*θ*</sub><sup> − 1</sup>∂*σ*<sub>*θ*</sub>/∂*S* are
constants set up to approximate the actual equation of state at the
desired target pressure, and over a desired range of *S* and *T*.

Since density in is given by the sum of a linear function of *S* plus a
linear function of *T*, a linear-scale slide-rule could be set up to do
this calculation. Extending this to a nonlinear equation of state would
involve writing where *F* and *G* are devised to approximate the full
equation of state over a reasonable application range.

Apparatus
=========

![Sketch of slide rule.](sigmat_idea.png)

Figure 1 illustrates the idea for a linear slide rule[1]. There are two
sliding scales. The lower one has *σ*<sub>*t*</sub> on the bottom side
and *S* on the top side. The upper one has *T* on the lower side.

Usage
=====

Given *S* and *T*, density *ρ* is calculated as follows.

1.  Slide the upper scale until the arrow at the left of the *T* scale
    lines up with the observed *S*.

2.  Slide the transparent overlay until its vertical line is on the *T*
    of interest.

3.  Determine *ρ* by tracing this vertical line down to the bottom
    scale.

The device can also be used in other ways, e.g. infer *T*, given *S* and
*ρ*.

Working notes
=============

First-order linear model
------------------------

Linear least-squares regression analysis (see `01.R`) on an 20 by 20
grid covering 28 &lt; *S* &lt; 36 and 0 &lt; *T* &lt; 20 yields with
*S*<sub>0</sub> = 28, *T*<sub>0</sub> = 0<sup>∘</sup>C, at pressure
*p* = 0dbar. The RMS misfit of this model is 0.177 kg/m**<sup>3</sup>,
and the maximum absolute misfit is 0.4504kg/m**<sup>3</sup>.

Second-order model
------------------

Second-order polynomial least-squares regression analysis (see `02.R`)
on an 20 by 20 grid covering 28 &lt; *S* &lt; 36 and 0 &lt; *T* &lt; 20
yields with *S*<sub>0</sub> = 28, *T*<sub>0</sub> = 0<sup>∘</sup>C, at
pressure *p* = 0dbar. The RMS misfit of this model is 0.034
kg/m**<sup>3</sup>, and the maximum absolute misfit is
0.1173kg/m**<sup>3</sup>.

Second-order model with cross term
----------------------------------

Second-order polynomial least-squares regression analysis (see `03.R`)
on an 20 by 20 grid covering 28 &lt; *S* &lt; 36 and 0 &lt; *T* &lt; 20
yields with *S*<sub>0</sub> = 28, *T*<sub>0</sub> = 0<sup>∘</sup>C,
*S*<sub>\*</sub> = 32, *T*<sub>\*</sub> = 10<sup>∘</sup>C, at pressure
*p* = 0dbar. The RMS misfit is 0.008 kg/m**<sup>3</sup>, and the maximum
absolute misfit is 0.0276kg/m**<sup>3</sup>.

Second-order model with cross-term and pressure-term
----------------------------------------------------

Second-order polynomial regression analysis, including pressure (see
`04.R`) on an 20**<sup>4</sup> grid covering the ranges
28 &lt; *S* &lt; 36, 0 &lt; *T* &lt; 20 and 0 &lt; *p* &lt; 400 yields
with *S*<sub>0</sub> = 28, *T*<sub>0</sub> = 0<sup>∘</sup>C,
*S*<sub>\*</sub> = 32, *T*<sub>\*</sub> = 10<sup>∘</sup>C. The RMS
misfit is 0.009 kg/m**<sup>3</sup>, and the maximum absolute misfit is
0.0336kg/m**<sup>3</sup>. (I also tried with a linear pressure term but
the coefficient was 2.06810^{-5}, which is only 0.0103 at 500dbar, so I
did not judge this correction important enough (for coastal work) to
justify adding a second correction graph.

Device consruction
==================

Run `sigma_theta_circular_.R` non-interactively, which creates files
named `sigma_theta_circular_1_lower.pdf` and
`sigma_theta_circular_1_upper.pdf`. Print the files, and laminate in
clear plastic to make reasonably rigid sheets. Trim each sheet along the
dotted gray line, thus forming semi-rigid disks. Then create a
tansparent radial pointer that can reach from the temperature axis to
the central point. Draw a straight line olong the centre of the pointer,
with permanent ink. Puncture the two laminated disks at the central
point (marked with a small circle) and also puncture the pointer along
its central line, at a spot that lets the pointer reach to the
temperature scale.

The result is a slide rule for calculating *σ*<sub>*θ*</sub>. To test
it, follow the directions printed on the top layer, and see if you get
the indicated test value.

[1] A circular slide rule could also be set up.
