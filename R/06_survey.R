# 5. Survey data. This is a bit more complicated, since we have multiple
# surveys for the same year. Therefore, the survey results are stored in a 
# three-dimensional array, Vaccine x Year x Survey Id.
#
# Analysis is in several steps
# - select subset of surveys that apply for child diseases
# - remove surveys with low information (e.g. sample size < 300)
# - correct "recall bias" in surveys, e.g. when 3rd shot rate is implausibly
#   high
# - remove surveys when working group decisions say so
# - average the available surveys for a given vaccine

# Survey results passed for inclusion in the analysis include:
# * card or history results
# * cohorts 12-23, 18-29, 15-26, 24-35 months of age
#
# Prolog
# survey_for_analysis(C, V, Y, ID, Description, Coverage) :-
#     survey_results0(C, V, Y, ID, Description, Coverage),
#     member(confirm:'card or history', Description),
#     member(age:AgeCohort, Description),
#     member(AgeCohort, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']).

# Reasons to exclude a survey include:
# * Sample size < 300,
# * The working group decides to exclude the survey
#
# Prolog
# survey_accepted(C, V, Y, ID, Coverage) :-
#     survey_for_analysis(C, V, Y, ID, Desc, Cov0),
#     (   decision(C, V, Y, acceptSurvey, _, ID, _)
#     ;   member(ss:Size, Desc),
#         Size >= 300,
#         not(decision(C, V, Y, ignoreSurvey, _, ID, _)),
#         not(decision(C, V, Y, ignoreSurvey, _, na, _))
#     ),
#     % Check if survey needs to be modified
#     (   survey_modified(C, V, Y, ID, _, Modified)
#     ->  Coverage = Modified
#     ;   Coverage = Cov0
#     ).

Idn = levels(as.factor(Survey$Id))
Svy.Ana = array(NA_integer_, dim=c(length(Yn()), length(Vn()), length(Idn)), 
                dimnames=list(Y=Yn(), V=Vn(), Id=Idn))

conf = Survey$Info.confirm == "card or history"
age  = Survey$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
size = Survey$Info.ss >= 300
index = Survey[conf & age & size, ]
Svy.Ana[cbind(index$Y, index$V, index$Id)] = index$Cov

index = Decisions[Decisions$Dec == "acceptSurvey", ]
Svy.Ana[cbind(index$Y, index$V, index$Id)] = index$Cov

index = Survey[conf & age & size, ]
Svy.Title = array(NA_character_, dim=c(length(Yn()), length(Vn()), length(Idn)), 
                  dimnames=list(Y=Yn(), V=Vn(), Id=Idn))
Svy.Title[cbind(index$Y, index$V, index$Id)] = index$Info.title

# Recall bias is estimated by comparing the first and third dose of a vaccine
#
# Prolog
# vaccine(dtp3, dtp1).
# vaccine(pol3, pol1).
# vaccine(hib3, hib1).
# vaccine(hepb3, hepb1).
# vaccine(pcv3, pcv1).
#
# survey_modified(C, V, Y, ID, Expl, Coverage) :-
#     member(V, [dtp3, pol3, hib3, hepb3, pcv3]),
#
#     % Third dose, card only
#     survey_results0(C, V, Y, ID, DescriptionCard3Dose, C3Cov),
#     member(confirm:card, DescriptionCard3Dose),
#     member(age:AgeCohortCard3Dose, DescriptionCard3Dose),
#     member(AgeCohortCard3Dose, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),

V13 = c(dtp1="dtp3", pol1="pol3", hib1="hib3", hepb1="hepb3", pcv1="pcv3")
vac = Survey$V %in% V13
cnf = Survey$Info.confirm == "card"
age = Survey$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
index = Survey[vac & cnf & age, ]

Card3 = array(NA_integer_, dim=c(length(Yn()), length(V13), length(Idn)), 
              dimnames=list(Y=Yn(), V=V13, Id=Idn))
Card3[cbind(index$Y, index$V, index$Id)] = index$Cov

#     % First dose, card or history
#     vaccine(V, First),
#     survey_results0(C, First, Y, ID, DescriptionCoH1Dose, CoH1Cov),
#     member(confirm:'card or history', DescriptionCoH1Dose),
#     member(age:AgeCohortCoH1, DescriptionCoH1Dose),
#     member(AgeCohortCoH1, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),

vac = Survey$V %in% names(V13)
cnf = Survey$Info.confirm == "card or history"
age = Survey$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
index = Survey[vac & cnf & age, ]

CH1 = array(NA_integer_, dim=c(length(Yn()), length(names(V13)), length(Idn)), 
            dimnames=list(Y=Yn(), V=names(V13), Id=Idn))
CH1[cbind(index$Y, index$V, index$Id)] = index$Cov

#     % First dose, card only
#     survey_results0(C, First, Y, ID, DescriptionCard1Dose, C1Cov),
#     C1Cov > 0,
#     member(confirm:card, DescriptionCard1Dose),
#     member(age:AgeCohortCard1Dose, DescriptionCard1Dose),
#     member(AgeCohortCard1Dose, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),

vac = Survey$V %in% names(V13)
cnf = Survey$Info.confirm == "card"
age = Survey$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
cv0 = Survey$Cov > 0
index = Survey[vac & cnf & age & cv0, ]

Card1 = array(NA_integer_, dim=c(length(Yn()), length(names(V13)), length(Idn)), 
              dimnames=list(Y=Yn(), V=names(V13), Id=Idn))
Card1[cbind(index$Y, index$V, index$Id)] = index$Cov

#     Adj is C3Cov / C1Cov,
#     ThirdHistoryAdj is (CoH1Cov - C1Cov) * Adj,
#     CovAdjusted is C3Cov + ThirdHistoryAdj,
#     bound_0_100(CovAdjusted, Cov0),

Adj = Card3 / Card1
H3Adj = Adj * (CH1 - Card1)
H3Adj[] = pmin(99, pmax(0, tround(Card3 + H3Adj)))

#     survey_for_analysis(C, V, Y, ID, Description, SurveyCoverage),
#     Cov0 \= SurveyCoverage,

# Todo: Should we use some minimum distance instead of rounding?
index = which(tround(Svy.Ana[, V13, , drop=FALSE]) != H3Adj, arr.ind=TRUE)

#     SurveyCovRounded is round(SurveyCoverage),
#     CH1 is round(CoH1Cov),
#     C1 is round(C1Cov),
#     C3 is round(C3Cov),
#     member(title:Title, Description),
#     concat_atom([Title, ' card or history results of ', SurveyCovRounded,
#         ' percent modifed for recall bias to ', Cov0,
#         ' percent based on 1st dose card or history coverage of ',
#         CH1, ' percent, 1st dose card only coverage of ',
#         C1, ' percent and 3rd dose card only coverage of ',
#         C3, ' percent. '], Expl),
#         Coverage = Cov0.

Svy.Info = array(NA_character_, dim=c(length(Yn()), length(Vn()), length(Idn)),
                 dimnames=list(Y=Yn(), V=Vn(), Id=Idn))

if(any(index))
{
  Svy.Info[, V13, ][index] = sprintf(
    "%s card or history results of %.0f percent modifed for recall bias to %.0f percent based on 1st dose card or history coverage of %.0f percent, 1st dose card only coverage of %.0f percent and 3rd dose card only coverage of %.0f percent. ", 
    Svy.Title[, V13, ][index], tround(Svy.Ana[, V13, ][index]), H3Adj[index],
    tround(CH1[index]), tround(Card1[index]), tround(Card3[index]))
  Svy.Ana[, V13, ][index] = H3Adj[index]
}

# Some surveys are ignored by the working group
index = Decisions[Decisions$Dec == "ignoreSurvey", ]
Svy.Acc = Svy.Ana # todo, collect explanations
Svy.Acc[cbind(index$Y, index$V, index$Id)] = NA
Svy.Info[cbind(index$Y, index$V, index$Id)] = NA

# Todo: Collect explanations for Svy.Ana here, not later

# % Survey information for given year. Multiple surveys are averaged.
# survey(C, V, Y, Expl, Coverage) :-
#     findall(Cov, survey_accepted(C, V, Y, _, Cov), [H | T]),
#     length([H | T], N),
#     sum_list([H | T], Sum),
#     Coverage is round(Sum / N),
#     concat_atom(['Survey evidence of ', Coverage, ' percent based on ',
#         N, ' survey(s). '], Expl).

# Svy.Cov = YV.int()
Svy.Cov = tround(apply(Svy.Acc, c(1, 2), mean, na.rm=TRUE))

Svy.Expl = YV.char()
Svy.Expl[] = sprintf(
  "Survey evidence of %g percent based on %i survey(s). ", 
  Svy.Cov, apply(!is.na(Svy.Acc), c(1, 2), sum))

# Reasons to exclude a survey include:
#   Sample size < 300,
#   The working group decides to exclude the survey.
# This is used later for explanation/4
#
# Prolog
# survey_reason_to_exclude(C, V, Y, ID, Expl) :-
#     survey_for_analysis(C, V, Y, ID, Description, _),
#     not(decision(C, V, Y, acceptSurvey, Expl, ID, _)),
#     member(ss:Size, Description),
#     Size < 300,
#     concat_atom(['Survey results ignored. Sample size ', Size,
#         ' less than 300. '], Expl).

# Todo: bgd, bcg/1999: twice the same message, without Survey ID
exclude = array(NA_real_, dim=c(length(Yn()), length(Vn()), length(Idn)), 
                dimnames=list(Y=Yn(), V=Vn(), Id=Idn))
conf = Survey$Info.confirm == "card or history"
age  = Survey$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
size = Survey$Info.ss < 300
index = Survey[conf & age & size, ]
exclude[cbind(index$Y, index$V, index$Id)] = index$Info.ss

index = Decisions[Decisions$Dec == "acceptSurvey", ]
exclude[cbind(index$Y, index$V, index$Id)] = NA

Svy.Excl = array("", dim=c(length(Yn()), length(Vn()), length(Idn)), 
                 dimnames=list(Y=Yn(), V=Vn(), Id=Idn))
Svy.Excl[] = ifelse(is.na(exclude), "", 
                    sprintf("Survey results ignored. Sample size %i less than 300. ", exclude))

Svy.Excl = apply(Svy.Excl, c(1, 2), paste, collapse="")

# Old code
# if(nrow(index))
#   for(i in 1:nrow(index))
#   {
#     if(!any(index$V[i] == accept$V & index$Y[i] == accept$Y & index$Id[i] == accept$Id))
#       Expl[index$Yn[i], index$V[i]] = sprintf(
#         "%sSurvey results ignored. Sample size %i less than 300. ", 
#         Expl[index$Yn[i], index$V[i]], index$Info.ss[i])
#   }
