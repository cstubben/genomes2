# genomes

`genomes` is an `R` package on Bioconductor that collects genome sequencing project data from NCBI using E-utility scripts and the ENA using REST queries.  A short description of the five basic E-utility scripts (einfo, esearch, esummary, efetch and elink) is provided below and many other functions and datasets are included within the package.  

The `einfo` function lists the 51 Entrez databases (accessed April 12, 2013).

	einfo()
	            DbName
	1         assembly
	2       bioproject
	3        biosample
	4       biosystems
	5      blastdbinfo
	6            books
	7              cdd
	...

You can also query a specific database to return the indexing fields available for searching

	einfo("taxonomy")[,1:4]
	   Name          FullName                                 Description TermCount
	1   ALL        All Fields        All terms from all searchable fields   9979456
	2  ALLN         All Names                    All aliases for organism   1494291
	3  COMN       Common Name                     Common name of organism     37400
	4  EDAT       Entrez Date Date record first accessible through Entrez      6088
	5  FILT            Filter                          Limits the records       353
	6    GC                GC                        Nuclear genetic code        16
	7  LNGE           Lineage              Lineage in taxonomic hierarchy   1494291
	8  MDAT Modification Date                         Date of last update      4969
	9   MGC               MGC                  Mitochondrial genetic code        33
	10 NXLV        Next Level     Immediate parent in taxonomic hierarchy    182957
	...

or set the links option to TRUE to list the available links (needed for elinks).

	einfo("genome", links=TRUE)[, 1:2]
	                        Name                      Menu
	1          genome_bioproject          BioProject Links
	2                genome_gene                Gene Links
	3             genome_nuccore                Components
	4 genome_nuccore_samespecies Other genomes for species
	5             genome_protein             Protein Links
	6     genome_proteinclusters     Protein Cluster Links
	7              genome_pubmed              PubMed Links
	8            genome_taxonomy            Taxonomy Links


The `esearch` command runs the Entrez database searches and returns the History Server details (query_key and WebEnv for subsequent calls).  For example, this command will find pubmed articles with bioconductor listed in the title (pubmed is the default database).

	esearch("bioconductor[TITLE]")
	[1] "92 results found"
	      db results query_key                                                  WebEnv
	1 pubmed      92         1 NCID_1_224456233_130.14.18.53_9001_1330229913_639962231

Set the usehistory option to "n" to return a vector of ids.  

	esearch("mouse", db="taxonomy", usehistory="n")
	[1] "10090"

Also, any additional key-value pairs may be included in the options, for example, this query finds pubmed articles published in the last year (see the NCBI [help](http://www.ncbi.nlm.nih.gov/books/NBK25499) pages for full details on the optional E-utility parameters).

	x <- esearch("bioconductor[TITLE]", reldate=360)

The objects returned by esearch may be passed directly to the remaining three function (esummary, efetch or elink). I recommned using the history object since this also reads the Entrez database name and you are not limited to 200 IDs (which is the limit requested by NCBI in the URL strings).  The `esummary` functions returns Entrez database summaries and includes a simple XML parser to return a data.frame (or set parse=FALSE to return the raw XML or optionally use the `ncbiPubmed` function to parse the raw XML and return a short citation with author, year, title, journal and published date).  You may also select the old XML format (default) or the newer Entrez version 2.0  (for display here I have selected only a few of the 42 columns available).

	esummary(x, version="2.0")[, c(1, 42, 6)]
	      PubDate SortFirstAuthor                                                                                                                       Title
	1  2013 Mar 2        Heider A                               virtualArray: a R/bioconductor package to merge raw data from different microarray platforms.
	2  2013 Mar 1     Schröder MS                                                   RamiGO: an R/Bioconductor package providing an AmiGO visualize interface.
	3 2012 Dec 24       Taminau J Unlocking the potential of publicly available microarray data using inSilicoDb and inSilicoMerging R/Bioconductor packages.
	...
	13 2012 Apr 24      Castro MA   RedeR: R/Bioconductor package for representing modular structures, nested networks and multiple levels of hierarchical...

The `efetch` command may be used to return records in a variety of formats and you should check the online [table](http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1) for a long list of valid retrieval types and modes for each database.   In addition, the `genomes` package includes a number of specific parsers to format efecth output, for example,  to parse submission dates from GenBank files using `ncbiSubmit` or taxonomy lineage using `ncbiTaxonomy`.  Many other parsers could be added such as reading feature tables into GRanges or FASTA files into Biostrings (some are included in the genomes2 package on Github).  In this example, efetch is used to return abstracts from the previous esearch query.

	efetch(x, rettype="abstract")
	  [1] ""                                                                                    
	  [2] "1. BMC Bioinformatics. 2013 Mar 2;14:75. doi: 10.1186/1471-2105-14-75."              
	  [3] ""                                                                                    
	  [4] "virtualArray: a R/bioconductor package to merge raw data from different"             
	  [5] "microarray platforms."                                                               
	  [6] ""                                                                                    
	  [7] "Heider A, Alt R."                                                                    
	  [8] ""                                                                                    
	  [9] "Translational Centre for Regenerative Medicine Leipzig, University of Leipzig,"      
	 [10] "Semmelweisstr, 14, Leipzig 04103, Germany. aheider@trm.uni-leipzig.de."              
	 [11] ""                                                                                    
	 [12] "BACKGROUND: Microarrays have become a routine tool to address diverse biological "   
	 [13] "questions. Therefore, different types and generations of microarrays have been"      
	 [14] "produced by several manufacturers over time. Likewise, the diversity of raw data "   
	 [15] "deposited in public databases such as NCBI GEO or EBI ArrayExpress has grown"        

         
The functions may be nested (esearch -> esummary) to summarize searches in a single step.

	esummary(esearch( "Yersinia pestis CO92[ORGN] AND refseq[FILTER] AND nuccore genome[Filter]", db="nuccore"))[, c(2,3,5,10)]
	[1] "4 results found"
	    Caption                                                 Title       Gi  Length
	1 NC_003143      Yersinia pestis CO92 chromosome, complete genome 16120353 4653728
	2 NC_003131  Yersinia pestis CO92 plasmid pCD1, complete sequence 16082691   70305
	3 NC_003134  Yersinia pestis CO92 plasmid pMT1, complete sequence 16082781   96210
	4 NC_003132 Yersinia pestis CO92 plasmid pPCP1, complete sequence 16082679    9612


If you are fetching sequences, use caution when downloading large sequences.  One option is to add a low seq_stop if you are not sure what will be returned in the search results.  

	efetch(esearch( "Yersinia pestis CO92[ORGN] AND refseq[FILTER] AND nuccore genome[Filter]", db="nuccore"), seq_stop=700, rettype="fasta")
	[1] "4 results found"
	 [1] ">gi|16120353:1-700 Yersinia pestis CO92 chromosome, complete genome"      "GATCTTTTTATTTAAACGATCTCTTTATTAGATCTCTTATTAGGATCATGATCCTCTGTGGATAAGTGAT"  
	 [3] "TATTCACATGGCAGATCATATAATTAAGGAGGATCGTTTGTTGTGAGTGACCGGTGATCGTATTGCGTAT"   "AAGCTGGGATCTAAATGGCATGTTATGCACAGTCACTCGGCAGAATCAAGGTTGTTATGTGGATATCTAC"  
	 [5] "TGGTTTTACCCTGCTTTTAAGCATAGTTATACACATTCGTTCGCGCGATCTTTGAGCTAATTAGAGTAAA"   "TTAATCCAATCTTTGACCCAAATCTCTGCTGGATCCTCTGGTATTTCATGTTGGATGACGTCAATTTCTA"  
	 [7] "ATATTTCACCCAACCGTTGAGCACCTTGTGCGATCAATTGTTGATCCAGTTTTATGATTGCACCGCAGAA"   "AGTGTCATATTCTGAGCTGCCTAAACCAACCGCCCCAAAGCGTACTTGGGATAAATCAGGCTTTTGTTGT"  
	 [9] "TCGATCTGTTCTAATAATGGCTGCAAGTTATCAGGTAGATCCCCGGCACCATGAGTGGATGTCACGATTA"   "ACCACAGGCCATTCAGCGTAAGTTCGTCCAACTCTGGGCCATGAAGTATTTCTGTAGAAAACCCAGCTTC"  
	[11] "TTCTAATTTATCCGCTAAATGTTCAGCAACATATTCAGCACTACCAAGCGTACTGCCACTTATCAACGTT"   ""                                                                        
	[13] ">gi|16082691:1-700 Yersinia pestis CO92 plasmid pCD1, complete sequence"  "GTGTAACGAACGGTGCAATAGTGATCCACACCCAACGCCTGAAATCAGATCCAGGGGGTAATCTGCTCTC"  
	[15] "CTGATTCAGGAGAGTTTATGGTCACTTTTGAGACAGTTATGGAAATTAAAATCCTGCACAAGCAGGGAAT"   "GAGTAGCCGGGCGATTGCCAGAGAACTGGGGATCTCCCGCAATACCGTTAAACGTTATTTGCAGGCAAAA"  
 
Similar to esearch, the `elink` command also returns either the History Server or a vector of ids if the cmd option is switched to neighbor (default is neighbor_history).                                                      

	elink("15718680,157427902", dbfrom="protein", db="gene")
	    db         link query_key                                                  WebEnv
	1 gene protein_gene         1 NCID_1_15399116_130.14.18.52_9001_1330231183_1240047351

	elink("15718680,157427902", dbfrom="protein", db="gene", cmd="neighbor")
	[1] 522311   3702

These commands can also be nested to return complicated queries in a single line (esearch-> elink -> esummary).  In this case, viral genomes linked to a Reference sequence in Entrez genome are displayed.  Many links are also available as filters, so this search in nuccore would also work `Nipah virus[ORGN] AND nuccore genome samespecies[Filter]`

	esummary( elink( esearch("Nipah virus", "genome"), db="nuccore", linkname="genome_nuccore_samespecies"))[, c(2,3,6,10)]
	[1] "1 result found"
	    Caption                                                           Title CreateDate Length
	1  JN808863          Nipah virus isolate NIVBGD2008RAJBARI, complete genome 2012/02/04  18252
	2  JN808857        Nipah virus isolate NIVBGD2008MANIKGONJ, complete genome 2012/02/04  18252
	3  FJ513078 Nipah virus isolate Ind-Nipah-07-FG from India, complete genome 2009/12/31  18252
	4  AY988601                    Nipah virus from Bangladesh, complete genome 2005/06/01  18252
	5  AJ627196          Nipah virus complete genome, isolate NV/MY/99/VRI-0626 2005/01/07  18246
	6  AJ564623           Nipah virus complete genome, isolate NV/MY/99/UM-0128 2004/01/05  18246
	7  AJ564622          Nipah virus complete genome, isolate NV/MY/99/VRI-1413 2004/01/05  18246
	8  AJ564621          Nipah virus complete genome, isolate NV/MY/99/VRI-2794 2004/01/05  18246
	9  AY029768                      Nipah virus isolate UMMC2, complete genome 2001/09/07  18246
	10 AY029767                      Nipah virus isolate UMMC1, complete genome 2001/09/07  18246


Check the help pages for additional details and please post any comments or issues.

