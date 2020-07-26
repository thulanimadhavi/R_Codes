#inclass assignment BT 4019
#s12764
#1
myCSVData = read.csv("portal_data_joined.csv")
n=sum((myCSVData$genus == "Neotoma" ) & (myCSVData$species == "albigula") & (myCSVData$year == 1990))
n
#2
i=1

vec=numeric(0)
a=numeric(0)
DNA = c("C","G","T","A","A")



for (i in 1:length(DNA)){
  
  
  if (DNA[i] == "C"){
   nuc = print("G")
  }
     else if(DNA[i] == "G"){
    nuc = print("C")
    }
        else if(DNA[i] == "A"){
    nuc = print("T")
      }
          else if(DNA[i] == "T"){
    nuc = print("A")
        }
  
  a[i]= DNA[i]
  vec[i]=c(nuc)
}
result= rbind(a,vec)
result

#complementSeq(c("C","G","T","A","A"))

#3
s=sample(c("A","C","G","T"),100000,replace = T)
myGC = function(DNA){
  gc = sum(DNA=="G" | DNA=="C")
  total = length(DNA)
  GC = gc/total*100
  print(GC)
}

myGC(s)
