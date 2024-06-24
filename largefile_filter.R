# https://www.deployhq.com/git/faqs/removing-large-files-from-git-history
git filter-branch --force --index-filter \
'git rm --cached --ignore-unmatch R/markdown_reports/Openspaces/Openspaces_rgb/sst2010sAverage_coldcolors.tif' \
--prune-empty --tag-name-filter cat -- --all