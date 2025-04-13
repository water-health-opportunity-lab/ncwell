# ncwell

**Code repository for the REACH Center pilot project**  
_Data-Driven Solutions to Mitigate the Impact of Hurricanes on Private Well Drinking Water Quality and Human Health_

This project develops reproducible, data-driven workflows to analyze the effects of hurricanes on private well contamination and associated health risks. We use public datasets, geospatial methods, and exploratory data analysis in R.

---

## 📁 Project Structure

```text
ncwell/
├── LICENSE               # License information
├── ncwell.Rproj          # RStudio project file
├── README.md            # Project documentation
└── script/              # Main analysis scripts
    ├── 1_access_data/       # Scripts to download or load raw data
    ├── 2_data_wrangling/    # Scripts for cleaning and transforming data
    └── 3_eda/               # Exploratory data analysis and initial visualizations
```

## 🔧 Requirements
This project uses R.
To begin, open `ncwell.Rproj` in RStudio and run scripts in the `script/` folder in order.

## 🔐 API Keys
Store your API keys (e.g., Census API) in an `.Renviron` file in your home directory:
```text
CENSUS_API_KEY=your_key_here
```
Do not commit your `.Renviron` file to Git.

## 📌 Project Status
This project is in active development under the REACH Center pilot grant.