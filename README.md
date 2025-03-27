# First Homework  

This repository contains the source code (`src`) and the generated PDF output (`out`) for the **First Homework** of the **Statistical Models** class.  

## Setup Instructions  

To ensure the project runs correctly, follow these steps after cloning the repository.  

### 1️⃣ Clone the Repository 
Open a terminal or RStudio and run:  

```sh
git clone https://github.com/tricarico672/SM_HW1
cd <your-repo>
```
### 2️⃣ Open R or RStudio and Install``` renv```
If renv is not already installed, install it with:

```r
install.packages("renv")
```

### 3️⃣ Restore the Project Environment

Once inside the project directory, run the following command in R:

```r
renv::restore()
```

This will:

- Read the renv.lock file.

- Install the exact package versions used in the project.

### 4️⃣ Open and Run the Analysis

Now, you can open the .qmd or script file in RStudio and execute it to replicate the results.

## ⚠️ Troubleshooting

- If ```renv::restore()``` fails, check that you have write permissions in the project directory.

- If you want to update dependencies, run ```renv::snapshot()``` after installing new packages.




