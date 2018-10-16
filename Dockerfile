# Copyright (c) Jupyter Development Team.
# Distributed under the terms of the Modified BSD License.
FROM gversmee/hail-notebook

LABEL maintainer="Jupyter Project <jupyter@googlegroups.com>"

COPY jupyter_notebook_config.py /home/jovyan/.jupyter/jupyter_notebook_config.py

USER root

RUN pip install --no-cache-dir dockerspawner \
jupyter \
lxml \
jupyter-spark \
google-cloud \
cloudstorage \
plotly \
gcsfs \
scikit-learn \
#scriptine \
devtools \
statsmodels \
datadog \
google-api-python-client \
selenium \
pyensembl \
https://github.com/khramts/assocplots/archive/master.zip \
jgscm && \
	fix-permissions /home/$NB_USER && \
	fix-permissions $CONDA_DIR
	
RUN conda install google-api-python-client rpy2 \
  r-devtools \
  r-rcurl \
	r-xml \
	r-data.table \
	r-httr \
	r-rlist \
	--quiet -y && \
  conda clean -tipsy && \
	rm -rf /home/$NB_USER/.local && \
  fix-permissions $CONDA_DIR && \
	fix-permissions /home/$NB_USER
	
#RUN Rscript -e "options(unzip = 'internal');.libPaths(c('/opt/conda/lib/R/library', '/usr/local/spark-2.3.1-bin-hadoop2.7/R/lib'))"

#ENV GEN_CERT "ok"
ENV JUPYTER_ENABLE_LAB "ok"
ENV TAR /bin/tar

COPY Rprofile.R /home/jovyan/.Rprofile
	
USER $NB_UID

WORKDIR $HOME

