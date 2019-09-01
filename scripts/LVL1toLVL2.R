# This script is a first-pass automated clean-up tool for in-situ turbidity data.

LVL1toLVL2 = function (Turbidity,ResetValue,TransitionValue,MaxChange,MaxValue){
  
  # Replace negative Values With NA, 0's with 1e-5, and delete anything above the set max value
  Turbidity$DataValue[Turbidity$DataValue < 0] = NA # Makes code more efficient and simpler
  Turbidity$DataValue[Turbidity$DataValue == 0] = 1e-5 # Avoids divide by zero errors
  Turbidity$DataValue[Turbidity$DataValue > MaxValue] = NA # This is the first cleaning tool
  
  # Setup some parameters used in the while loop
  CurrentChange = as.data.frame(matrix(nrow = nrow(Turbidity), ncol = 1))
  CurrentChange[1:5,1] = 0
  
  # This will iterate through
  for (i in 6:nrow(Turbidity)) {
    
    # Check if value is blank and replace with a predicted value
    if (is.na(Turbidity$DataValue[i]) == TRUE){
      placeholder = 1:5
      model = lm(Turbidity$DataValue[(i-5):(i-1)] ~ placeholder) 
      if (model$coefficients[1] + 6*model$coefficients[2] < 0){
        Turbidity$DataValue[i] = Turbidity$DataValue[i-1]
      } else {
        Turbidity$DataValue[i] = model$coefficients[1] + 6*model$coefficients[2]
      }
      CurrentChange[i,1] = 100*(Turbidity$DataValue[i]-Turbidity$DataValue[i-1])/(Turbidity$DataValue[i-1])
      
      # Change QA code to 1 (Estiamted Value)
      Turbidity$QualifierID[i] = 1
      
      # Reset condition
    } else if (Turbidity$DataValue[i] < ResetValue) {
      
      CurrentChange[i,1] = 100*(Turbidity$DataValue[i]-Turbidity$DataValue[i-1])/(Turbidity$DataValue[i-1])
      
      # In case this point returns from bad data, reset the currentchange value to 0
      if (abs(CurrentChange[i,1]) > 100){
        CurrentChange[i,1] = 0
      }
      # Check if the value is less than the transition value  
    } else if (Turbidity$DataValue[i] < TransitionValue) {
      
      # Calculate the percent change for the current value
      CurrentChange[i,1] = 100*(Turbidity$DataValue[i]-Turbidity$DataValue[i-1])/(Turbidity$DataValue[i-1])
      
      # Calculate actual difference between current and previus value, accounting for number of time steps
      AbsoluteDiffernce = abs((Turbidity$DataValue[i]-Turbidity$DataValue[i-1]))
      
      # Calculate difference between past percent change and current percent change
      SlopeDifference = abs(CurrentChange[i,1] - CurrentChange[i-1,1])
      
      # If the absolute change is less than 50%, less than 5 NTU, or change in slope is
      # less than 30%, accept the value
      if (abs(CurrentChange[i,1]) < 50 || AbsoluteDiffernce < 5 || SlopeDifference < 50){
        
        # Otherwise, reject the value (unless predicted value is higher)
      } else {
        placeholder = 1:5
        model = lm(Turbidity$DataValue[(i-5):(i-1)] ~ placeholder) 
        
        # If predicted value is less than zero, just repeat previous value
        if (model$coefficients[1] + 6*model$coefficients[2] < 0){
          Turbidity$DataValue[i] = Turbidity$DataValue[i-1]
          
          # Change QA code to 1 (Estiamted Value)
          Turbidity$QualifierID[i] = 1
          
          # If measured value is less than predicted, keep measured    
        } else if (model$coefficients[1] + 6*model$coefficients[2] > Turbidity$DataValue[i]){
          
          # Otherwise, use predicted value 
        } else {
          Turbidity$DataValue[i] = model$coefficients[1] + 6*model$coefficients[2]
          
          # Change QA code to 1 (Estiamted Value)
          Turbidity$QualifierID[i] = 1
        }
        CurrentChange[i,1] = 100*(Turbidity$DataValue[i]-Turbidity$DataValue[i-1])/(Turbidity$DataValue[i-1])
      }
      
      # Value is above TransitionValue
    } else {
      # Calculate the percent change for the current value
      CurrentChange[i,1] = 100*(Turbidity$DataValue[i]-Turbidity$DataValue[i-1])/(Turbidity$DataValue[i-1])
      
      # Calculate difference between past percent change and current percent change
      SlopeDifference = abs(CurrentChange[i,1] - CurrentChange[i-1,1])
      
      # Calculate absolute difference between current and previus value, accounting for number of time steps
      AbsoluteDiffernce = abs((Turbidity$DataValue[i]-Turbidity$DataValue[i-1]))
      
      # If the difference in slopes is less than 50 and the absolute change isn't greater than 50 per 15 min,
      # accept the value as real. This is mainly to find real high values.
      if (SlopeDifference < 50 & AbsoluteDiffernce < MaxChange) {
        
      # Otherwise, the value is not real
      } else {
        placeholder = 1:5
        model = lm(Turbidity$DataValue[(i-5):(i-1)] ~ placeholder) 
        
        # If predicted value is less than zero, just repeat previous value
        if (model$coefficients[1] + 6*model$coefficients[2] < 0){
          Turbidity$DataValue[i] = Turbidity$DataValue[i-1]
          
          # Change QA code to 1 (Estiamted Value)
          Turbidity$QualifierID[i] = 1
          
          # If measured value is less than predicted, keep measured    
        } else if (model$coefficients[1] + 6*model$coefficients[2] > Turbidity$DataValue[i]){
          
          # Otherwise, use predicted value 
        } else {
          Turbidity$DataValue[i] = model$coefficients[1] + 6*model$coefficients[2]
          
          # Change QA code to 1 (Estiamted Value)
          Turbidity$QualifierID[i] = 1
        }
        CurrentChange[i,1] = 100*(Turbidity$DataValue[i]-Turbidity$DataValue[i-1])/(Turbidity$DataValue[i-1])
      }
    }
  }
  
  # Remove sections where linear interpoaltion goes beyond 3 points
  Index = NA
  for (i in 1:nrow(Turbidity)){
    if (Turbidity$QualifierID[i] == 0) {
      Index[i] = 0
    } else {
      Index[i] = Index[i-1] + 1
    }
  }
  
  for (i in 1:nrow(Turbidity)) {
    if (Index[i] > 3){
      Index[i:(i-Index[i]+1)] = -1
    }
  }
  Turbidity$DataValue[Index == -1] = NA
  
  # Undo the 0 value change from earlier and repalce NA with -9999
  Turbidity$DataValue[Turbidity$DataValue == 1e-5] = 0
  Turbidity$DataValue[is.na(Turbidity$DataValue)] = -9999
  #write.csv(cbind(Turbidity),"TurbidityResults.csv", na = "")
  
  # Print success message
  message("Run successful, with ",nrow(Turbidity[Turbidity$QualifierID==1,]), " (",
          round(nrow(Turbidity[Turbidity$QualifierID==1,])/nrow(Turbidity)*100,digits = 2),"%) values replaced.")
  return(Turbidity)
}

