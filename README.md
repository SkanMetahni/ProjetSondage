# TunSample Pro 🇹🇳 📊

**TunSample Pro** est une application web interactive développée avec **R Shiny**. Elle est conçue pour automatiser et simplifier les processus d'échantillonnage statistique et l'analyse de données, avec une interface optimisée pour les utilisateurs et le contexte tunisien.

## 🚀 Fonctionnalités principales

- **📁 Importation de données** : Chargement et lecture de fichiers Excel (.xlsx).
- **🎲 Méthodes d'échantillonnage** : 
    - Échantillonnage Aléatoire Simple (SAS).
    - Échantillonnage Stratifié (avec allocation proportionnelle).
- **🧪 Validation Statistique** : 
    - Tests de représentativité automatisés (Test du Chi-deux / $\chi^2$).
    - Comparaison automatique entre l'échantillon et la population cadre.
- **📈 Visualisation interactive** : Graphiques dynamiques pour analyser la structure des strates et des échantillons.
- **📥 Exportation** : Génération et téléchargement immédiat des échantillons au format Excel.

## 🛠️ Installation et Lancement

Pour exécuter ce projet localement sur votre machine :

1. **Prérequis** : Installez [R](https://cran.r-project.org/) et [RStudio](https://posit.co/download/rstudio-desktop/) (ou utilisez VS Code avec l'extension R).
2. **Installation des packages** : Lancez la commande suivante dans votre console R :
   ```r
   install.packages(c("shiny", "shinydashboard", "readxl", "writexl", "ggplot2", "dplyr", "plotly"))

📝 Structure technique du projet
Le projet repose sur un script unique de plus de 3600 lignes, structuré comme suit :
🖥️ UI (User Interface) : Une interface moderne basée sur shinydashboard avec des menus intuitifs.
⚙️ Server : Une logique backend robuste gérant les calculs statistiques complexes et la manipulation de données.
🧬 Moteur Statistique : Algorithmes d'échantillonnage développés sur mesure pour garantir la précision mathématique.
👥 Auteur
Développé par SkanMetahni dans le cadre d'un projet d'expertise en sondage et statistiques.
Ce projet est sous licence MIT - libre d'utilisation et de modification.
---