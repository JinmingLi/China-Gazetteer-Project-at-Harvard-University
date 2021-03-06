
# coding: utf-8

# In[ ]:

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.rcParams['font.sans-serif']=['SimHei']
plt.rcParams['axes.unicode_minus']=False

#### Encapsulate all the function ####

class Check(object):
    
    
    def __init__(self, path, exclude={'Province', 'name', 'county','county_1986', 'year', 'd', '服用药物_Unit',
         '结扎_Unit', '结扎男_Unit', '结扎女_Unit', '引产_Unit', '放环_Unit', '人流_Unit', '节育_Unit',
        '节育男_Unit', '节育女_Unit', '工资总额_Unit', '工资总额全民_Unit', '工资总额集体_Unit', '人口密度_Unit',
        '职工平均工资集体_Unit', '职工平均工资全民_Unit', '职工平均工资_Unit','总户数_Unit', '总人口_Unit'}, 
                 units={('county', 'county_1986'),
                 ('人口密度', '人口密度_Unit'),
                 ('人流', '人流_Unit'),
                 ('工资总额', '工资总额_Unit'),
                 ('工资总额全民', '工资总额全民_Unit'),
                 ('工资总额集体', '工资总额集体_Unit'),
                 ('引产', '引产_Unit'),
                 ('总人口', '总人口_Unit'),
                 ('总户数', '总户数_Unit'),
                 ('放环', '放环_Unit'),
                 ('服用药物', '服用药物_Unit'),
                 ('结扎', '结扎_Unit'),
                 ('结扎女', '结扎女_Unit'),
                 ('结扎男', '结扎男_Unit'),
                 ('职工平均工资', '职工平均工资_Unit'),
                 ('职工平均工资全民', '职工平均工资全民_Unit'),
                 ('职工平均工资集体', '职工平均工资集体_Unit'),
                 ('节育', '节育_Unit'),
                 ('节育女', '节育女_Unit'),
                 ('节育男', '节育男_Unit')}, 
                 id_column=['name','county','year'], 
                 items='county', 
                 index='year'):
                    self.path = path
                    self.exclude = exclude
                    self.units = units
                    self.id_column = id_column
                    self.items = items
                    self.index = index
                    self.data =  pd.read_excel(self.path)
        

        
    def check_type(self):
        
        for column in set(self.data.columns.values).difference(self.exclude):
            if any(self.data[column].notnull()):
                if str in set(self.data[column].apply(type)):
                    print(column)
            
 
            
    def check_type_print(self):
        for column in set(self.data.columns.values).difference(self.exclude):
            if any(self.data[column].notnull()):
                for i, cell in self.data[column].iteritems():
                    if type(cell) == str:
                        print(list(self.data.iloc[i][self.id_column+[column]]), column)



                    
    def check_missing_unit(self):
        for var, unit in self.units:
            if any(self.data[var].notnull() & self.data[unit].isnull()):
                print(var)
                
                
                
    def plot(self, county, var):
        self.data[['year',var]][self.data['county'] == county].plot.scatter(x='year', y=var, color='blue')
        plt.show()


 
    def plot_all(self):
            for item in set(self.data[self.items]):
                for column in set(self.data.columns.values).difference(self.exclude):
                    if any(self.data[column][self.data[self.items] == item].notnull()):
                        try:
                            self.data[[self.index, column]][self.data[self.items] == item].plot.scatter(x=self.index, y=column, color='blue')
                            plt.show()
                            print(item)
                        except KeyError and ValueError:
                            pass
                



    def show_data(self, item, column):
        return self.data[[self.index, column]][self.data[self.items] == item]
        
    
    
    def correct_data(self, item, value_index, var, value):
        self.data.loc[(self.data[self.items] == item) & (self.data[self.index] == value_index), var] = value
        print('this entry has been corrected')
        
    def export_data(self, export_path):
        self.data.to_excel(export_path, index=False)

#### Input the data from excel ####

date = 1030
name = 'Jinming Li'
p = Check('C:/Users/.../data_entry_{0}_{1}.xlsx'.format(date, name))


#### Check data type error  ####

p.check_type_print()        #### call the check_type_print function to check data type error such as "6..50" or "4a30" ####
county = 'Linwu county'     #### correct the data error ####
var = 'death rate'
year = 1983
value =6.50

data.loc[(data['county'] == county) & (data['year'] == year), var] = value 
print('Finished')

#### Check variables which do not have units ####

p.check_missing_unit()

#### Plot all the data with year in x-axis and variables in y-axis to detect outliers ####

p.plot_all()

#### Plot the county data when outliers are found ####

p.plot('Linwu county', 'deathrate')

#### Show all the data in county that has outliers ####

p.show_data() 

#### Correct the outliers ####

p.correct_data('Linwu county', '1985', 'deathrate', '6.50')

#### Export the data file after data checking process ####

p.export_data('C/Users/Jinming Li/Desktop/...')

