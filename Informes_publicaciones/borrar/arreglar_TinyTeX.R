
#arreglar tinytex 
tinytex:::install_prebuilt()



tinytex::parse_install(
  text = "! LaTeX Error: File `multirow.sty' not found."
)

tinytex::tlmgr_install("multirow")
