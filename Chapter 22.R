###### Chapter 22 Creating dynamic reports
# There are three common report scenarios. 
# In the first, you create a report that includes your code and the results, 
#     so that you can remember what you did six months from now. It’s easier 
#     to reconstruct what was done from a single comprehensive document 
#     than from a set of related files.
# In the second scenario, you have to generate a report for a teacher, a 
#     supervisor, a client, a government agency, an internet audience, or a 
#     journal editor. Clarity and attractiveness matter, and the report may
#     only need to be created once. 
# In the third scenario, you need to generate a specific type of report on a 
#     regular basis. It may be a monthly report on product or resource usage,
#     a weekly financial analysis, or a report on web traffic that’s updated 
#     hourly. In any case, the data changes, but the analyses and the
#     structure of the report remain the same.
####
# Additionally, the data can be tied to the report so that changing the data 
#     changes the report. These dynamic reports can be saved as
# ■ Web pages
# ■ Microsoft Word documents 
# ■ Open Document files
# ■ Publication-ready PDF or PostScript documents
####
# Dynamic documents and reproducible research
# There is a powerful movement growing within the academic community in 
#     support of reproducible research. The goal of reproducible research is 
#     to facilitate the replication of scientific findings by including the
#     data and software code necessary to reproduce findings with the 
#     publications that report them. 
# This allows readers to verify the findings for themselves and gives them an
#     opportunity to build on the results more directly in their own work. 
#     The techniques described in this chapter, including the embedding of 
#     data and source code with documents, directly support this effort.
####
# how R is included in the report is controlled by the options.
# The template file (example.Rmd) is a plain text file containing three 
#     components:
# ■ Report text—Any explanatory phrases and text. Here, the report text is Report,
#     Here is some data, Plots, and Here is a plot.
# ■ Formatting syntax—The tags that control report formatting. In this file, 
#     Markdown tags are used to format the results. Markdown is a simple 
#     markup language than can be used to convert plain text files to 
#     structurally valid HTML or XHTML. The pound symbol # in the first line
#     isn’t a comment. It produces a level-1 header. ## produces a level-2 
#     header, and so on.
# ■ R code—R statements to be executed. In R Markdown documents, R code
#     chunks are surrounded by ```{r} and ```. The first code chunk lists the
#     first six rows of the dataset, and the second code chunk produces a 
#     scatter plot. In this example, both the code and the results are output 
#     to the report, but options allow you to control what’s printed on a 
#     chunk-by-chunk basis.
####
#  R Markdown templates can be used to create HTML, PDF, and MS Word documents.
# ODT and DOCX templates are used to create Open Document and Microsoft Word 
#     documents, respectively.
# LaTeX templates are used to create publication-quality PDF documents,
#     including reports, articles, and books. 
####
require("rmarkdown")
render("women.Rmd", "html_document")
render("women_latex.Rmd", "pdf_document")
render("women_doc.Rmd", "word_document")
# Note that you had to replace the sfit object with sfit$coefficients. The 
#     xtable() function can handle lm() objects, but the kable() function can
#     only handle matrices and data frames. 
####
require("knitr")
knit("drugs_sample.Rnw")
#  By default, knit("drugs.Rnw") inputs the file drugs.Rnw and outputs the 
#     file drugs.tex. The .tex file is then run through a LaTeX compiler, 
#     creating a PDF, PostScript, or DVI file. 
knit2pdf("drugs_sample.Rnw") # The function generates the .tex file and converts it 
#     to a finished PDF document named drugs.pdf. 
####















