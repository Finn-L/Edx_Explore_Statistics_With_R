## source("http://www.bioconductor.org/biocLite.R") 
## After installing bioconductor
## biocLite("GEOquery")
## biocLite("limma") 
## install.packages("gplots")
library(Biobase)
library(GEOquery)
library(limma) 
library(gplots)
## load the dataset
gds <- getGEO('GDS1542', destdir=".")
## basic information
class(gds)
show(gds)
## convert data to general functions of Bioconductor
eset <- GDS2eSet(gds)
show(eset)
## extract actual values
expdata <- exprs(eset)
dim(expdata)
head(expdata)

## identify any missing value
sum(is.na(expdata))
w <- which(apply(is.na(expdata), 1, sum) > 0)
## filter away that row
temp <- expdata[-w, ]
dim(temp)
expdata <- temp
boxplot(as.data.frame(expdata))
## transform data
logdata <- log2(expdata)
boxplot(as.data.frame(logdata))

##  we want to eliminate as many variables as possible that 
## we do not consider biologically meaningful 
probemeans <- apply(logdata, 1, mean)
probesd <- apply(logdata, 1, sd)
plot(probemeans, probesd)
## It is clear that standard deviation is higher at lower means. 
## Some probes have veryy high standard deviations - these may be truly changing, 
## something we will investigate further on.
## Let's first eliminate the 25% of the probes that have weakest signal:
q25 <- quantile(probemeans, 0.25)
whichtosave <- which(probemeans > q25)
q25logdata <- logdata[whichtosave,]
## remove those with very low variability
mydata <- q25logdata[apply(q25logdata, 1, IQR) > 1.5, ]
dim(mydata)[1]

## do a Principal Component Analysis (PCA)
## use the function prcomp() for the PCA. It does the analysis between rows.

## "transpose" the matrix
tdata <- t(mydata)

pca <- prcomp(tdata, scale=T)
summary(pca)
plot(pca$x, type="n")
text(pca$x, rownames(pca$x), cex=0.5)
## see which experimental condition they belong to
conditions <- phenoData(eset)$agent
plot(pca$x, type="n")
text(pca$x, labels=conditions, cex=0.5)
## do a dendrogram of correlation between the samples.
pearsonCorr <- as.dist(1 - cor(mydata))
hC <- hclust(pearsonCorr)
plot(hC, labels = sampleNames(eset))
plot(hC, labels = conditions)
heatmap(mydata, col=greenred(100))

## Linear Models for Microarrays
condfactor <- factor(eset$agent)
## Construct a model matrix, and assign the names to the columns
design <- model.matrix(~0+condfactor)
colnames(design) <- c("ctrl", "tnf")
design
## estimates the variances
fit <- lmFit(eset, design)
## define the conditions we want to compare
contrastmatrix <- makeContrasts(tnf - ctrl,levels=design)
## calculate the p-values for the differences between the conditions defined by
## the contrast matrix
fit <- contrasts.fit(fit, contrastmatrix)
ebayes <- eBayes(fit)
ebayes
hist(ebayes$p.value)

## Benjamini-Hochberg adjustment
results <- decideTests(ebayes)
## display the number of probes that passed
sum(results == 1 | results == -1)
## length(which(results != 0))

################################################
## Data exploration continued
################################################
##A neat function to display the result of just two conditions is the Venn diagram:
vennDiagram(results)

## Extract the original data for the most changing probes:
        
        resData <- exprs(eset)[results != 0,]

## Add gene symbols as row names:
geneSymbol <- as.array(fData(eset)[,"Gene symbol"])
gs <- geneSymbol[c(which(results != 0))]
rownames(resData) <- gs

## Add p-values in an extra column:
        
pvalues <- ebayes$p.value[results != 0,]
resData <- cbind(resData, pvalues)

## And add-p values corrected for multiple testing (q-values):
        
adj.pvalues <- p.adjust(ebayes$p.value, method="BH")
adj.pvalues <- adj.pvalues[results != 0]
resData <- cbind(resData, adj.pvalues)

## Write output to a file:
write.table(resData, "most_regulated.txt", sep="\t")