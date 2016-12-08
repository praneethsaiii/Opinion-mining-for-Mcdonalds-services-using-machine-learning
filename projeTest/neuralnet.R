library(sets)

U1 <- seq(from = 0, to = 1, by = 0.0001)

#DIMENSIONS

variables <-
  
  set(
    
    code =
      fuzzy_partition(varnames= 
                        c(ELow = 0.2 , ENormal = 0.5, EHigh = 0.8),
                      FUN= fuzzy_cone, radius = 0.2, universe=U1),
    
    
    bandwidth =
      fuzzy_partition(varnames=
                        c(SLow = 0.2, SNormal=0.5, SHigh=0.8),
                      FUN = fuzzy_cone, radius = 0.2, universe=U1),
    
    
    acceleration =
      fuzzy_partition(varnames=
                        c(ALow = 0.2, ANormal=0.5, AHigh=0.8),
                      FUN = fuzzy_cone, radius = 0.2, universe=U1),
    
    
    processing =
      fuzzy_partition(varnames=
                        c(Local = 0.3, Remote = 0.7),
                      FUN = fuzzy_cone, radius = 0.3, universe=U1) 
    
    
  )


rules <-
  set(
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SHigh && acceleration %is% AHigh, processing %is% Remote),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SHigh && acceleration %is% ANormal, processing %is% Remote),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SHigh && acceleration %is% ALow, processing %is% Local),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SNormal && acceleration %is% AHigh, processing %is% Remote),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SNormal && acceleration %is% ANormal, processing %is% Remote),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SNormal && acceleration %is% ALow, processing %is% Local),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SLow && acceleration %is% AHigh, processing %is% Remote),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SLow && acceleration %is% ANormal, processing %is% Local),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SLow && acceleration %is% ALow, processing %is% Local),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SHigh, processing %is% Remote),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SLow, processing %is% Local),
    
    fuzzy_rule(code %is% EHigh && bandwidth %is% SNormal, processing %is% Local),
    
    fuzzy_rule(code %is% ENormal && bandwidth %is% SLow, processing %is% Local),
    
    fuzzy_rule(code %is% ENormal && bandwidth %is% SHigh, processing %is% Remote),
    
    fuzzy_rule(code %is% ENormal && bandwidth %is% SNormal, processing %is% Local),
    
    fuzzy_rule(code %is% ELow && bandwidth %is% SLow, processing %is% Local),
    
    fuzzy_rule(code %is% ELow && bandwidth %is% SNormal, processing %is% Local),
    
    fuzzy_rule(code %is% ELow && bandwidth %is% SHigh, processing %is% Local)
    
  )


context <- fuzzy_system(variables, rules)

print(context)
#plot(context)

#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912))
#0.7

#fi <- fuzzy_inference(context, list(code=0.5212, bandwidth=0.2121))
#0.3

#fi <- fuzzy_inference(context, list(code=0.1532, bandwidth=0.9321))
#0.3

#fi <- fuzzy_inference(context, list(code=0.2432, bandwidth=0.9323))
#0.3

#fi <- fuzzy_inference(context, list(code=0.4723, bandwidth=0.9542))
#0.7

#fi <- fuzzy_inference(context, list(code=0.2932, bandwidth=0.9484))
#0.3

#fi <- fuzzy_inference(context, list(code=0.2912, bandwidth=0.4324))
#0.3

#fi <- fuzzy_inference(context, list(code=0.2134, bandwidth=0.2194))
#0.3

#fi <- fuzzy_inference(context, list(code=0.4323, bandwidth=0.5345))
#0.3

#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912, acceleration = NA))
#There is a rule code is High AND bandwidth is High -> Remote. 
#as well as a rule, code is High AND bandwidth is High and acceleration is High -> Remote.
#0.7

#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912, acceleration = 0.93))
#0.7

#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.912, acceleration = 0.13))
#0.5

#fi <- fuzzy_inference(context, list(code=0.8123, bandwidth=0.2354, acceleration = 0.9243))
#0.4517523

#fi <- fuzzy_inference(context, list(code=0.4723, bandwidth=0.9542, acceleration=NA))
#This rule exists for the two variables provided
#0.7

#fi <- fuzzy_inference(context, list(code=0.8857, bandwidth=0.1194, acceleration=0.8921))
#0.4961897

fi <- fuzzy_inference(context, list(code=0.8857, bandwidth=0.1194, acceleration=0.1184))
#0.3


#dev.new()
#plot(fi)

gset_defuzzify(fi, "centroid")


U1 <- NULL