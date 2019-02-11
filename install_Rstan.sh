# Add marutter's c2d4u repository, (and rrutter for CRAN builds too)
sudo add-apt-repository -y "ppa:marutter/rrutter"
sudo add-apt-repository -y "ppa:marutter/c2d4u"
sudo apt-get update
sudo apt-get install r-cran-rstan
