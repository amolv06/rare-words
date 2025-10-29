import pandas as pd
import sqlite3

df=pd.read_excel("Frequency List.xlsx", sheet_name="The List - Frequency List")
df.rename(columns={'Frequency List' : "LEMMA|POS",
                   'Unnamed: 1' : 'LEMMA',
                   'Unnamed: 2' : 'POS',
                   'Unnamed: 3' : 'FREQUENCY',
                   'Unnamed: 4' : 'INFLECTIONS'}, inplace=True)
df = df.iloc[1:-1]
lemma_freq_df = df.groupby('LEMMA', as_index=False)['FREQUENCY'] \
                  .sum().rename(columns={'FREQUENCY': 'sum_freq'}) \
                  .sort_values('sum_freq', ascending=False)
lemma_freq_df['rank'] = lemma_freq_df['sum_freq'].rank(method='min',
                                                  ascending=False).astype(int)
lemma_freq_df["LEMMA"] = lemma_freq_df["LEMMA"].str.lower()
lemma_forms_df = df.assign(INFLECTIONS=df['INFLECTIONS'].fillna('')  
                 .astype(str).str.split(r'\s*,\s*')).explode('INFLECTIONS')
lemma_forms_df["LEMMA"] = lemma_forms_df["LEMMA"].str.lower()
lemma_forms_df["INFLECTIONS"] = lemma_forms_df["INFLECTIONS"].str.lower()
lemma_forms_df = lemma_forms_df.loc[lemma_forms_df['INFLECTIONS'] != ''] \
                               .drop_duplicates(['LEMMA', 'INFLECTIONS'])
forms_rank_df = lemma_forms_df.merge(lemma_freq_df, on='LEMMA', how='left') \
                              .rename(columns={'INFLECTIONS': 'form'}) \
                              [['form', 'LEMMA', 'sum_freq', 'rank']]
inflection_to_rank = (
    forms_rank_df.sort_values('sum_freq', ascending=False)
                 .drop_duplicates('form', keep='first')
                 .set_index('form')['rank'].to_dict()
                 )

con = sqlite3.connect("words.db")
cur = con.cursor()
cur.execute("create table dictionary(word varchar(999), rank int);")
with open("words_alpha.txt") as f:
    for x in f:
        cur.execute(f'insert into dictionary values ("{x.strip()}", '
                    f'{inflection_to_rank.get(x.strip(), 9999999)})')
con.commit()
    
