
################# download from espa ####################################################################

#install.packages("espa.tools", repos="http://R-Forge.R-project.org")
downloadpath <- paste0(L8datpath, "espa_order_downloads/")

earthexplorer_download(usgs_eros_username="MaiteLezama",
                       usgs_eros_password="Eos300dmmmmlv",
                       output_folder = downloadpath, verbose=T,
                       ordernum = "espa-mlezamavaldes@gmail.com-08102020-074516-892")