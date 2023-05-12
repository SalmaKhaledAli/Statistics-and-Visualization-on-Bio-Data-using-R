set.seed(50)

# 1. G1_control : 1000 samples following the standard normal distribution.
G1_control <- rnorm(1000)

# 2. G1_condition: 1000 samples following the standard normal distribution.
G1_condition <- rnorm(1000)

# 3. G2_control : 1000 samples following the normal distribution with mean = 2 and standard deviation = 0.5.
G2_control <- rnorm(1000, 2, 0.5)

# 4. G2_condition: 1000 samples following the normal distribution with mean = 4 and standard deviation = 0.5.
G2_condition <- rnorm(1000, 4, 0.5)

# 5. G3_control : 1000 samples following the normal distribution with mean = 2 and standard deviation = 0.5.
G3_control <- rnorm(1000, 2, 0.5)

# 6. G3_condition: 1000 samples following normal distribution with mean = 0 and standard deviation = 1.5.
G3_condition <- rnorm(1000, 0, 1.5)

# 7. G4_control : 1000 samples following the standard normal distribution.
G4_control <- rnorm(1000)

# 8. G4_condition: 1000 samples following the uniform distribution with min = 0 and max = 1.
G4_condition <- runif(1000)

####### Testing Hypotheses #######

hypotheses_testing<- function(Gene_control, Gene_condition, case, alpha = 0.05, alternative, type){
  # check if the hypotheses on one sample or two samples
  if(type == "One"){
    score <- 0.2
    if(shapiro.test(Gene_control)$p.value > alpha){
      t.test(x= Gene_control, mu= score, alternative= alternative)
    }else{
      wilcox.test(Gene_control, mu= score, alternative)
    }
    
  }else{
    # check if the samples is paired or independent
    if(case == TRUE){
      
      
      # Get the difference
      difference.samples <- Gene_condition - Gene_control
      #check for the normality of the distribution
      if (shapiro.test(difference.samples)$p.value > alpha){
        t.test(Gene_condition, Gene_control, paired= case, alternative= alternative)
        
      }else{
        wilcox.test(Gene_condition, Gene_control, alternative= alternative, paired= case)
        
      }
      
    }else{
      #check for the normality
      if(shapiro.test(Gene_condition)$p.value > alpha && shapiro.test(Gene_control)$p.value){
        #check for variance equality
        if(var.test(Gene_control, Gene_condition)$p.value >= alpha){
          t.test(x= Gene_control, y= Gene_condition, paired= case, alternative= alternative, var.equal= TRUE)
        }else{
          t.test(x= Gene_control, y= Gene_condition, paired= case, alternative= alternative, var.equal= FALSE)
        }
        
      }else{
        wilcox.test(x= Gene_control, y= Gene_condition, alternative= alternative)
      }
    }
  }
}
      ##############################################################################################################################################
# 1. For G1, check whether its GE level is the same under the normal state and the conditioned state assuming samples are paired.
print("Gene 1 Results Report")
print(hypotheses_testing(G1_control, G1_condition, TRUE, alpha = 0.05, 'two.sided', "Two"))

# 2. For G2, check whether its GE level is greater under the conditioned state assuming samples are independent.
print("Gene 2 Results Report")
print(hypotheses_testing(G2_condition, G2_control, FALSE, alpha = 0.05, 'g', "Two"))

# 3. For G3, check whether its GE level is greater under the conditioned state assuming samples are independent.
print("Gene 3 Results Report")
print(hypotheses_testing(G3_condition, G3_control, FALSE, alpha = 0.05, 'g', "Two"))

# 4. For G4, check whether its GE level is the same under the normal state and the conditioned state assuming samples are independent.
print("Gene 4 Results Report")
print(hypotheses_testing(G4_control, G4_condition, FALSE, alpha = 0.05, 'two.sided', "Two"))

# 5. For G4, check if the GE level of 0.2 is significantly different from any of its control values.
print(hypotheses_testing(G4_control, G4_condition, FALSE, alpha = 0.05, 'two.sided', "One"))

# Save the results in a txt file
Data <- capture.output(print("Gene 1 Results Report"),
                        print(hypotheses_testing(G1_control, G1_condition, TRUE, alpha = 0.05, 'two.sided', "Two")),
                        print("Gene 2 Results Report"),
                        print(hypotheses_testing(G2_condition, G2_control, FALSE, alpha = 0.05, 'g', "Two")),
                        print("Gene 3 Results Report"),
                        print(hypotheses_testing(G3_condition, G3_control, FALSE, alpha = 0.05, 'g', "Two")),
                        print("Gene 4 Results Report"),
                        print(hypotheses_testing(G4_control, G4_condition, FALSE, alpha = 0.05, 'two.sided', "Two")),
                        print("Gene 4 Results Report on control values"),
                        print(hypotheses_testing(G4_control, G4_condition, FALSE, alpha = 0.05, 'two.sided', "One"))
                        
                        )
writeLines(Data, con = file("C:/Users/Salma/Downloads/Assignement_03_file.txt"))

