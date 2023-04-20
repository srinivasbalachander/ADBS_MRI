# Load important packages for parallel processing

library(foreach)
library(doParallel)


# Specifications for parallel processing
n.cores <- parallel::detectCores() - 4

my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

foreach::getDoParWorkers()


# Make a few specifications for which atlas, type of motion/nuisance correction, etc

atlas = "brainnetomeCombined" # can be "power2011" or "schaefer2011Combined" or "brainnetomeCombined"

if(atlas == "brainnetomeCombined") {str2rem = "Brainnetome_"}
if(atlas == "power2011")  {str2rem = "Power2011_"}
if(atlas == "schaefer2011Combined")  {str2rem = "Schaefer2018_17Networks_"}       

atlas.labels = read.table(Sys.glob(paste0("atlases/*", atlas, "*.txt" )))
atlas.labels$V2 = gsub(str2rem, "", atlas.labels$V2)

correction_type = "ICA-AROMA" # can be "ICA-AROMA" or "aCompCor" or "Both"

if(correction_type == "ICA-AROMA") {cortype = "corrMatrix"}
if(correction_type == "aCompCor")  {cortype = "corrMatrix1"}
if(correction_type == "Both")  {cortype = "corrMatrix2"} 

ses = 101
mat_type = "correlation_matrix" # can be "correlation_matrix" or "covariance_matrix"


# Get subject list from HalfPIPE output folder
subj.list <- Sys.glob(paste0("mats/sub-", "*","_ses-", ses, "*", 
                               cortype, "_atlas-", atlas, 
                               "_desc-", mat_type, ".tsv" ))

subj.list <- gsub(".*sub-(.+)_ses.*", "\\1", subj.list)

#  Get the demographic data first!

demog.all <- read.csv("Phase1.csv")

demog <- demog.all[demog.all$subjectID %in% subj.list,]

# Read the matrix and give proper labels
conn.long <- data.frame(x.roi = rep(atlas.labels$V2, each = length(atlas.labels$V2)), 
                        y.roi = rep(atlas.labels$V2, times = length(atlas.labels$V2)),
                        roi.roi = NA, r = NA, index = NA,
                        x.roi.index = rep(1:length(atlas.labels$V2), each = length(atlas.labels$V2)),
                        y.roi.index = rep(1:length(atlas.labels$V2), times = length(atlas.labels$V2)))

conn.long$roi.roi <- paste0(conn.long$x.roi, ".", conn.long$y.roi)

# Make these indexes such that duplicates & diagonals are removed, so that computation time is halved
conn.long <- conn.long[conn.long$x.roi.index < conn.long$y.roi.index,]

conn.long$index <- 1:nrow(conn.long)

conn.long <- conn.long[,c("index", "x.roi", "y.roi", "roi.roi", "r")]


# Make each subject's matrix into long format 
conmat.proc <- function(subj) {x <- read.table(Sys.glob(paste0("mats/sub-", subj,"_ses-", ses, "*", 
                                                              cortype, "_atlas-", atlas, 
                                                              "_desc-", mat_type, ".tsv" )))
                                x[x == "NaN"] = NA
                                
                                rownames(x) <- atlas.labels$V2
                                colnames(x) <- atlas.labels$V2
                                  
                                x2 <- conn.long
                                
                                for(i in atlas.labels$V2){
                                  for(j in atlas.labels$V2){
                                    x2[x2$x.roi == i & x2$y.roi == j, "r"] <- x[i,j]
                                  }
                                }
    
                            return (x2)
                              }

# Do the analysis - just an example to run in parallel

foreach(i = conn.long$index) %dopar% {
  j = sqrt(i)
  print(j)
}
