
sjv.inespecific.death <- c("XB30","XP15","XP19","XP25","XP30")
#XP15 = antibiotikabehandling, profylax

sjv.inespecific.repro <- c("XB66","XB69","XP40","XP41","XP42","XP62","XP63")

sjv.gris.allm <- c("002","010","011","013","017","020","090","091",
                      "AA009","AA01301","AA0129","AA01291","AA0134","AA031","AA002","AA0021")

sjv.gris.circ <- c("100","AA911")

sjv.gris.resp <- c("120","121","123","125","132","133","134",
                   "AA015","RA014","RA4199","RA4111","RB41","RB42","RB4192")

sjv.gris.infek <- c("300","360","361","365","366","367","368","370","371","377",
                        "430","431","438","439","454",
                    "AA0120","AA405","AA4051","AA40513","AA40514","AA40515","AA40517","AA40519","AA40520","AA40530",
                    "AA40668","AA40669","AA40677","AA4068","AA4314")

sjv.gris.gift <- c("212","570","571",
                   "MM2201","AA111","AA1111")

sjv.gris.repro <- c("601","602","603","605",
                    "610","615","619","670","680","689","690","691","692","697",
                    "KA0142","KA0143","KA0144","KA0182",
                    "KA90","KA91","KA235","KA49","KB014","KB49","KC014","KC91","KC0112","KC401")

sjv.gris.nerv <- c("838","NJ91")

sjv.gris.oron <- c("865","?B41")

sjv.gris.hud <- c("905","907","HA41","HA42")

combined = rbind(data.frame(
  code = c(
    sjv.inespecific.death,
    sjv.inespecific.repro,
    sjv.gris.allm,
    sjv.gris.circ,
    sjv.gris.resp,
    sjv.gris.infek,
    sjv.gris.gift,
    sjv.gris.repro,
    sjv.gris.nerv,
    sjv.gris.oron,
    sjv.gris.hud
  ),
  class = c(
    rep("Unspecific death",
        length(sjv.inespecific.death)),
    rep("Unspecific reproductive",
        length(sjv.inespecific.repro)),
    rep("General",
        length(sjv.gris.allm)),
    rep("Circulatory",
        length(sjv.gris.circ)),
    rep("Respiratory",
        length(sjv.gris.resp)),
    rep("Infection",
        length(sjv.gris.infek)),
    rep("Poison",
        length(sjv.gris.gift)),
    rep("Reproductive",
        length(sjv.gris.repro)),
    rep("Nervous",
        length(sjv.gris.nerv)),
    rep("Ears",
        length(sjv.gris.oron)),
    rep("Skin",
        length(sjv.gris.hud))
  )
))

fwrite(comb.df,
       file = "I:/ESS/SVA3D/PRRS/sjv.diagnosis.classes.csv",
       sep = ";")

