\name{read.ncbi.ftp}
\alias{read.ncbi.ftp}
\title{  Read files from the NCBI genomes FTP  }
\description{ Read files from the NCBI genomes FTP  }
\usage{
read.ncbi.ftp(org, filePattern = "ptt$|rnt$", ftp = "genomes/archive/old_refseq/Bacteria", ...)
}
\arguments{
  \item{org}{ organism directory (new FTP requires species and refseq assembly) }
  \item{filePattern}{  load files matching a specific pattern, default is protein and rna tables}
  \item{ftp}{  name of base FTP directory }
  \item{\dots}{ other options passed to read functions }
}
\details{ This function reads files in the genomes FTP and loads sequence files 
(faa=protein, fna=genone, ffn=gene, frn=rna) using Biostring functions or converts 
coordinate files (gff, ptt, rnt, GeneMarkHMM, Glimmer, Prodigal)  to GRanges }

\value{ a Biostring or GRanges object }
%\references{ }
\author{ Chris Stubben }
\note{ The genomes FTP site was updated on Dec 2, 2015 \url{ftp://ftp.ncbi.nlm.nih.gov/genomes/README.txt} and the default directory is now genomes/archive/old_refseq/Bacteria   }

\seealso{ \code{\link{read.gff}}, \code{\link{read.ptt}}, \code{\link{read.genemark}}, \code{\link{read.glimmer}}, \code{\link{read.prodigal}} }
\examples{


\dontrun{


org<-"Burkholderia_pseudomallei_K96243_uid57733"
read.ncbi.ftp(org)         # Protein and rna tables
read.ncbi.ftp(org, "gff")  # GFF
read.ncbi.ftp(org, "fna")  # Genome sequences

read.ncbi.ftp(org, "Prod") # Prodigal annotations
read.ncbi.ftp(org, "Glim") # Glimmer

}


}
\keyword{ file }
