import pandas as pd

df = pd.read_csv("~/Documents/schoolwork/soda502/class2_20220830/black names.csv")

# Not so useful
db.describe

# access columns
df["name"]

# create columns using similar syntax
df["intercept"] = 1

# sort by a column
df = df.sort_values(by = "percent_black", ascending = False)

# summarise
summary = df.groupby("black_name").mean()

# filter
black_names = df[(df["percent_black"] > 30) & (df["percent_white"] < 30)]
df_new3 = df[df.name != "tyrik"]

# apply a function to a column
df["percent_black_new"] = df["percent_black"].apply(lambda x: x ** 2)

# drop a column
df_new = df.drop("percent_black", axis = 1)
df_new2 = df.drop(640)
