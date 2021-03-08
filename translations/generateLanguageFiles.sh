#!/bin/bash

MODULE_NAMES=${1:-"jaspTestModule"}
PATH_TO_R_FILE=$2
LANGUAGE_CODES=${3:-"nl de es pt ja tr"}

for moduleName in ${MODULE_NAMES[@]}
do
  echo Module: ${moduleName}
  lupdate -locations none -extensions cpp,qml -recursive ${moduleName} -ts ${moduleName}/po/QML-${moduleName}.pot ; 
  msgattrib --no-obsolete --no-location ${moduleName}/po/QML-${moduleName}.pot -o ${moduleName}/po/QML-${moduleName}.pot;

  for languageCode in ${LANGUAGE_CODES[@]} 
  do 
    echo Generating language File: ${languageCode}; 
    lupdate -locations none -extensions cpp,qml -recursive ${moduleName} -ts ${moduleName}/po/QML-${languageCode}.po ; 
    msgattrib --no-obsolete --no-location ${moduleName}/po/QML-${languageCode}.po -o ${moduleName}/po/QML-${languageCode}.po ; 
    lrelease ${moduleName}/po/QML-${languageCode}.po -qm ${moduleName}/inst/qml/translations/${moduleName}-${languageCode}.qm ; 
  done
  echo Rscript ${PATH_TO_R_FILE}/translate.R ${moduleName}
  Rscript ${PATH_TO_R_FILE}/translate.R ${moduleName};
done