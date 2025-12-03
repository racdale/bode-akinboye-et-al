# bode-akinboye-et-al
DyAD data processing and analysis pipeline for CRQA analysis in Bode-Akinboye, Rietdijk, Dale, & Turkstra (under review).

The research used "Dynamic Analysis of Discourse" (DyAD) point-and-click interface to view transcripts, check analysis, etc. The core components of this workflow are a set of processing and analysis functions in R. These are included here, and reflect the "under the hood" processing in DyAD.

*Note*: DyAD contains partial analysis for semantic and syntactic annotations of transripts as well. These are included as illustrations in the interface, though they remain under development. The primary analysis in the paper is at the lexical level which is the most complete part of the DyAD interface.

*Command-line version*: The interested reader can also consult `cli_analysis.R`. Led by the first author and hosted here by the third, this script checks DyAD's output and develops a standalone script that conducts analysis without the need to install the full DyAD interface for the lexical approach described in the paper.

Illustration of DyAD:

![DyAD illustration](dyad.jpg)
