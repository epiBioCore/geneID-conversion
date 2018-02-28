#!/usr/bin/env Rscript



suppressMessages(library("optparse"))

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="Input text file with gene identifiers to convert. File must contain only one column with no header", metavar="File"),
  make_option(c("-o", "--out"), type="character", default="out.txt", metavar="File",
              help="Name of output file name [default= %default]"),
 # make_option(c("-d","--description"),type="logical",default=TRUE,
 #             help="to add a description of each gene to output file [default=%default]"),
  make_option(c("-a","--annotation"),type="character",default=NULL, metavar="STR",
              help="The annotation you would like to use"),
  make_option(c("-I","--inputIDs"),type="character",default=NULL, metavar="STR",
              help="The type of gene identifiers** you want to convert ex. refseq_id, ensembl_gene_id")
  # make_option(c("-O","--output-ids"),type="character",default=NULL, metavar="STR",
  #             help="The type of gene identifiers** you want to convert to ex. external_gene_name")
); 

###epilogue for options
epilogue<-"Available annotations:
hg19
hg38
mm10

**Acceptable values for gene ids:
refseq_id \t Refseq transcript id(s) ex. NM_001271017
ensembl_gene_id \t Ensembl gene id(s) ex. ENSG00000000003
ensembl_transcript_id \t Ensembl transcript id(s) ex.ENST00000000233
ensembl_transcript_id_version \t Ensembl transcript id(s) versioned ex. ENST00000000233.1 
ucsc_id \t  UCSC id(s) ex.  uc007aet.1
mgi_id \t MGI id(s) ex. MGI:96285 
"

usage<-":ID_conversion.R -f genes.txt -a hg19 -I ensembl_gene_id  -o converted_gene_ids.txt"

 description<-"
 This program will take a text file of gene ids (either ensembl, refseq or ucsc ids) and convert them to gene symbols. The output of this program is   
 a tab-delimited text file with the original ids, the converted ids, and a description of each gene."
opt_parser = OptionParser(option_list=option_list,
                          epilogue=epilogue,
                          description=description,
                          usage=usage)
opt = parse_args(opt_parser);

if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

if (is.null(opt$annotation)) {
  print_help(opt_parser)
  stop("The annotation must be supplied",call.=FALSE)
}

if (is.null(opt$inputIDs)) {
  print_help(opt_parser)
  stop("Input geneIDs must be supplied",call.=FALSE)
}

# if (is.null(opt$to)) {
#   print_help(opt_parser)
#   stop("The output geneIDs  must be supplied",call.=FALSE)
# }


dat<-read.delim(opt$file,header=F,stringsAsFactors=F,na.strings=c("",NA))
if (ncol(dat)!=1) {
stop("Gene ID file must only have 1 column")
}

if (sum(is.na(dat))>0) {
warning("NAs were found in input gene list. NAs will be removed")
}

ids<-dat$V1[!is.na(dat$V1)]

####check that values of annotation, from are acceptable

##
annot<-opt$annotation

valid_annot<-c("hg19","mm10","hg38")

if (!opt$annotation %in% valid_annot) {
  stop("Annotation is not currently available. Available annotatations are: hg19,hg38 and mm10")
}

annot<-opt$annotation

  lookup_file_path<-paste0("/local_storage/annotation_db/Gene_name_lookup_tables/",annot,"_gene_name_lookup_table.txt")
  lookup_file<-read.delim(lookup_file_path,header=T)

###check if from and to values are valid
valid_ids<-c("refseq_id","ensembl_transcript_id_version","ensembl_transcript_id","ensembl_gene_id","ucsc_id","mgi_id")

if (!opt$inputIDs %in% valid_ids) {
stop("Input id type is not valid. Please see help to find list of valid gene ids.")
}

###check that if mgi ids are the input id, that mm10 is the annotation
if (opt$inputIDs=="mgi_id" & opt$annotation != "mm10") {
stop("mgi ids are only available for mm10 annotation")
}



if (opt$inputIDs=="refseq_id") {
  cols<-c("refseq_id","refseq_gene_symbol","description")
} else if (opt$inputIDs %in% c("ensembl_transcript_id_versioned","ensembl_transcript_id","ensembl_gene_id","ucsc_id")) {
  cols<-c(opt$inputIDs,"ensembl_gene_symbol","description")
} else if (opt$inputIDs=="mgi_id") {
  cols<-c(opt$inputIDs,"mgi_symbol","description")
}

lookup_col<-lookup_file[,opt$inputIDs]
conversion<-lookup_file[match(ids,lookup_col),cols]
colnames(conversion)[2]<-"gene_symbol"

##change gene symbol colname?
write.table(conversion,file=opt$out,sep="\t",row.names=FALSE,quote=FALSE)
