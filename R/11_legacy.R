# Flag modifications in the program code that change the coverage estimates

wuenic.legacy = function(Bounded, Legacy)
{
  # Prolog
  # change_from_previous(C, V, Y, Coverage, Change) :-
  #     legacy(C, V, Y, Legacy),
  #     Legacy \= Coverage,
  #     !,
  #     concat_atom(['Estimate of ', Coverage,
  #         ' percent changed from previous revision value of ',
  #         Legacy,' percent. '], Change).
  #
  # change_from_previous(_C, _V, _Y, _, '').
  
  Change = YV.char("")
  index = which(Bounded != Legacy)
  Change[index] = sprintf(
    "Estimate of %i percent changed from previous revision value of %i percent. ",
    Bounded[index], Legacy[index])

  list(Change=Change)  
}