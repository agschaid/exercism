-module(rna_transcription).

-export([to_rna/1]).

to_rna(Strand) -> [to_rna_nucleotide(N) || N <- Strand].

to_rna_nucleotide($G) -> $C;
to_rna_nucleotide($C) -> $G;
to_rna_nucleotide($T) -> $A;
to_rna_nucleotide($A) -> $U.



