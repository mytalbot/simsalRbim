# Check if there is perfect preference symmetry
if(sum(ydata$optionA > ydata$quantityB) - dim(ydata)[1]==0){
  warning("Wow! You have perfect preference symmetry! \n There is no need for a model, but you can have the plot anyway. Yay!!!\n")
}else{}
