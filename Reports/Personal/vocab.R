seventh_grade <-  c(
  "abdicate", 	"connotation", 	"legendary",
  "abrasive", 	"consecutive", 	"liaison",
  "abruptly", 	"irrelevant", 	"libel",
  "acknowledge", 	"consult", 	"ludicrous",
  "acquire", 	"contrast", 	"mandatory",
  "addict", 	"copious", 	"mitigate",
  "adequate", 	"correspond", 	"naive",
  "admonish", 	"dawdle", 	"narrate",
  "affiliation", 	"deceitful", 	"necessity",
  "agitate", 	"demeanor", 	"negligent",
  "allege", 	"derogatory", 	"obnoxious",
  "allocate", 	"devastate", 	"omit",
  "alternative", 	"devious", 	"opposition",
  "amendment", 	"devour", 	"oppress",
  "antagonize", 	"diversity", 	"perceive",
  "attribute", 	"eligible", 	"persuasive",
  "authentic", 	"emphasize", 	"prediction",
  "bamboozle", 	"estimate", 	"prominent",
  "belligerent", 	"evaluate", 	"prospective",
  "bewilder", 	"bewildered", 	"exonerated",
  "punctual", 	"exposition", 	"quote",
  "bias", 	"exuberant", 	"relinquish",
  "boycott", 	"famished", 	"resolve",
  "condor", 	"formidable", 	"rudimentary",
  "cause", 	"impartial", 	"signify",
  "characterize", 	"indifferent", 	"sovereign",
  "chronological", 	"industrious", 	"suspense",
  "compel", 	"inevitable", 	"talisman",
  "competent", 	"infuriate", 	"tentative",
  "component", 	"inhabitants", 	"toxic",
  "conclusive", 	"initiate", 	"treason",
  "concur", 	"intimidate", 	"viewpoint",
  "condemn", 	"irate", 	"confront"
)


Rstudied7th <- c(
  "exuberant",
  "devour",
  "obnoxious",
  "addict",
  "condemn",
  "liaison",
  "relinquish",
  "ludicrous",
  "punctual",
  "acknowledge",
  "exuberant",
  "legendary",
  "exonerated",
  "evaluate",
  "toxic",
  "conclusive",
  "abrasive",
  "allege",
  "tentative",
  "condor",
  "formidable",
  "derogatory",
  "mitigate",
  "inhabitants",
  "prominent",
  "bewildered",
  "initiate",
  "demeanor",
  "sovereign",
  "eligible",
  "attribute", 
  "characterize",
  "boycott",
  "agitate",
  "infuriate",
  "bamboozle",
  "connotation",
  "abruptly",
  "dawdle",
  "antagonize",
  "oppress" 
)

Rlist7th <- setdiff(seventh_grade, Rstudied7th)

sample(Rlist7th, 10)

eighth_grade <- c(
  "abhor", 	"construct", 	"precise",
  "abrasive", 	"contrast", 	"prediction",
  "alternative", 	"corroborate", 	"prevalent",
  "ambiguous", 	"depict", 	"procedure",
  "amiss", 	"derive", 	"profound",
  "anarchy", 	"despicable", 	"proprietor",
  "anonymous", 	"despondent", 	"prudent",
  "anthology", 	"elapse", 	"pseudonym",
  "apathy", 	"embark", 	"quote",
  "apprehend", 	"encompass", 	"rebel",
  "assimilate", 	"endeavor", 	"rebuff",
  "assumption", 	"evidence", 	"rebuke",
  "audacious", 	"evoke", 	"recur",
  "authority", 	"feasible", 	"resilient",
  "avid", 	"focus", 	"response",
  "ban", 	"formula", 	"reverberate",
  "belligerent", 	"generation", 	"significant",
  "bisect", 	"gruesome", 	"similar",
  "bizarre", 	"imminent", 	"simulate",
  "boycott", 	"impel", 	"simultaneous",
  "capable", 	"imperative", 	"source",
  "cause", 	"integrate", 	"specific",
  "characterize", 	"interrogate", 	"spontaneous",
  "chronological", 	"merge", 	"surmise",
  "commence", 	"modify", 	"theory",
  "compels", 	"mutiny", 	"tirade",
  "concise", 	"narrate", 	"universal",
  "conclude", 	"novice", 	"validate",
  "confiscate", 	"obsolete", 	"variable",
  "conjecture", 	"opposition", 	"conscientious",
  "perish", 	"consecutive", 	"perspective",
  "consistent", 	"persuasive", 	"inspire"
)

list8th <- setdiff(eighth_grade, seventh_grade)

Rstudied8th <- c("words")

Pstudied8th <- c("words")

Rlist8th <- setdiff(list8th, Rstudied8th)

Plist8th <- setdiff(list8th, Pstudied8th)

sample(Rlist8th, 10)

sample(Plist8th, 10)

ninth_grade <- c(
  "absolve", 	"escalate", 	"mediate",
  "alleviate", 	"evaluate", 	"mortify",
  "alternative", 	"exacerbate", 	"niche",
  "ambivalent", 	"excerpt", 	"obscure",
  "analyze", 	"exemplify", 	"obsolete",
  "animosity", 	"explicit", 	"pacify",
  "approximate", 	"exposition", 	"perception",
  "arbitrary", 	"falter", 	"perspective",
  "attribute", 	"feasible", 	"pertinent",
  "beneficial", 	"feign", 	"ponder",
  "comprehensive", 	"fluctuate", 	"prevalent",
  "connotation", 	"formulate", 	"proponent",
  "contrast", 	"generate", 	"punitive",
  "credible", 	"gist", 	"rapport",
  "cursory", 	"hypothetical", 	"rationale",
  "cynic", 	"impartial", 	"reconcile",
  "dearth", 	"implausible", 	"redundant",
  "deficient", 	"implication", 	"respective",
  "demonstrate", 	"imply", 	"retaliate",
  "depict", 	"incentive", 	"sabotage",
  "derive", 	"incoherent", 	"scrutiny",
  "detract", 	"indolent", 	"simulate",
  "devastate", 	"infamous", 	"squander",
  "digress", 	"infuriate", 	"succumb",
  "dilemma", 	"innovation", 	"tangible",
  "diligent", 	"intercede", 	"technique",
  "dissent", 	"interpret", 	"traumatic",
  "distort", 	"intimidate", 	"turmoil",
  "diversion", 	"isolate", 	"valid",
  "elation", 	"jeopardize", 	"verify",
  "elicit", 	"lucrative", 	"viable",
  "elude", 	"mandatory", 	"vulnerable"
)

list9th <- setdiff(ninth_grade, seventh_grade)

list9th <- setdiff(list9th, eighth_grade)

Pstudied9th <- c(
  "redundant",
  "scrutiny",
  "exacerbate",
  "exposition",
  "alternative",
  "fluctuate",
  "deficient",
  "elicit",
  "illicit",
  "innovation",
  "analyze",
  "formulate",
  "intercede",
  "cursory",
  "interpret",
  "dissent",
  "beneficial",
  "elude",
  "mediate",
  "animosity",
  "pertinent",
  "turmoil",
  "implausible",
  "rationale",
  "falter",
  "demonstrate",
  "succumb",
  "vulnerable",
  "generate",
  "incentive" 
)

Plist9th <- setdiff(list9th, Pstudied9th)

sample(Plist9th, 10)

tenth_grade <- c(
  "abstract", 	"divert", 	"oppose",
  "admonish", 	"dormant", 	"panacea",
  "advocate", 	"egocentric", 	"perfunctory",
  "alternative", 	"elusive", 	"preposterous",
  "ambiguous", 	"emulate", 	"precarious",
  "analogy", 	"equitable", 	"precipitate",
  "anarchy", 	"eradicate", 	"preclude",
  "assiduous", 	"estrange", 	"proficient",
  "assimilate", 	"exacerbate", 	"propensity",
  "augment", 	"expedite", 	"qualitative",
  "authentic", 	"fabricate", 	"quantitative",
  "belligerent", 	"facilitate", 	"recalcitrant",
  "bolster", 	"fortuitous", 	"redeem",
  "bureaucratic", 	"fraudulent", 	"rejuvenate",
  "circumvent", 	"heinous", 	"relegate",
  "coalition", 	"hypothetical", 	"relinquish",
  "cohesive", 	"illicit", 	"repugnant",
  "collaborate", 	"imminent", 	"resilient",
  "comply", 	"impetuous", 	"retrospect",
  "concurrent", 	"incongruous", 	"sanction",
  "connotation", 	"indigenous", 	"spontaneous",
  "constituent", 	"indiscriminate", 	"static",
  "contingent", 	"inherent", 	"stringent",
  "criteria", 	"jurisdiction", 	"subordinate",
  "demeanor", 	"lax", 	"subsidize",
  "deplore", 	"meticulous", 	"tenuous",
  "derogatory", 	"negligent", 	"travesty",
  "disparity", 	"nonchalant", 	"tumult",
  "disseminate", 	"oblivious", 	"unilateral",
  "dissident", 	"obscure", 	"validate",
  "distraught", 	"omnipotent", 	"vindicate",
  "docile", 	"opportune", 	"zealot"
)

sample(tenth_grade, 10)
















