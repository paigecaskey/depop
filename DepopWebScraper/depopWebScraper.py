import time
import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException
from selenium.common.exceptions import NoSuchElementException

def findValues(searchname, numValues):
    chrome_options = webdriver.ChromeOptions()
    data = []
    #driver = webdriver.Chrome(options=chrome_options) 
    driver = webdriver.Chrome(executable_path='/Users/paigecaskey/Desktop/chromedriver', options=chrome_options)
    driver.get('https://www.depop.com/search/?q=' + searchname + '&sort=newlyListed')
    try:
        accept_cookies_button = driver.find_element(By.XPATH, '/html/body/div/div/div[2]/div[2]/button[2]')
        accept_cookies_button.click()
    except NoSuchElementException:
        pass 
    scroll_height = driver.execute_script("return document.documentElement.scrollHeight;")
    window_height = driver.execute_script("return window.innerHeight;")
    current_scroll_position = 0

    visited_links = set()
    visited_indices = set()

    while current_scroll_position < scroll_height and len(data) < int(numValues):
        driver.execute_script("window.scrollTo(0, {});".format(current_scroll_position))
        time.sleep(1)
    
        elements = driver.find_elements(By.CLASS_NAME, 'styles__ProductCard-sc-369aefb3-4')
    
        for idx, element in enumerate(elements):
            if idx in visited_indices:
                continue
            try:
                href_value = element.get_attribute('href')

                if href_value not in visited_links:
                   visited_links.add(href_value)

                try:
                    driver.execute_script("arguments[0].scrollIntoView();", element)
                    time.sleep(2)
                    element.click()
                    time.sleep(2)
                    internal_time_element = driver.find_element(By.CLASS_NAME, 'styles__Time-sc-630c0aef-0')
                    datetime_value = internal_time_element.get_attribute('datetime')
                    price = driver.execute_script('''
                            var priceElement = document.querySelector("#main > div.styles__Layout-sc-170b29e9-2.jTEyFL > div.styles__ContentWrapper-sc-170b29e9-3.kuRFFw > div.ProductDetailsSticky-styles__Wrapper-sc-72aea6c3-0.lmrJfz.styles__StyledProductDetailsSticky-sc-170b29e9-14.cfvkxK > div.ProductDetailsSticky-styles__DesktopKeyProductInfo-sc-72aea6c3-9.fXjXhB > div.ProductDetailsSticky-styles__StyledProductPrice-sc-72aea6c3-4.iprVwO > div > p");
                            return priceElement ? priceElement.innerText : 'NULL';
                            ''')
                    description = driver.execute_script('''
                            var descriptionElement = document.querySelector("#main > div.styles__Layout-sc-170b29e9-2.jTEyFL > div.styles__ContentWrapper-sc-170b29e9-3.kuRFFw > div.styles__Container-sc-141e0806-0.jdBUzQ.styles__StyledDescription-sc-170b29e9-17.kowUgV");
                            return descriptionElement ? descriptionElement.innerText : 'NULL';
                            ''')
                    brand = driver.execute_script('''
                        var brandElement = document.querySelector("#main > div.styles__Layout-sc-170b29e9-2.jTEyFL > div.styles__ContentWrapper-sc-170b29e9-3.kuRFFw > div.ProductDetailsSticky-styles__Wrapper-sc-72aea6c3-0.lmrJfz.styles__StyledProductDetailsSticky-sc-170b29e9-14.cfvkxK > div.ProductDetailsSticky-styles__DesktopKeyProductInfo-sc-72aea6c3-9.fXjXhB > div.ProductAttributes-styles__Attributes-sc-303d66c3-1.dIfGXO.ProductDetailsSticky-styles__StyledProductAttributes-sc-72aea6c3-10.kJTavv > a");
                        return brandElement ? brandElement.innerText : 'NULL';
                        ''')
                    condition = driver.execute_script('''
                        var conditionElement = document.querySelector("#main > div.styles__Layout-sc-170b29e9-2.jTEyFL > div.styles__ContentWrapper-sc-170b29e9-3.kuRFFw > div.ProductDetailsSticky-styles__Wrapper-sc-72aea6c3-0.lmrJfz.styles__StyledProductDetailsSticky-sc-170b29e9-14.cfvkxK > div.ProductDetailsSticky-styles__DesktopKeyProductInfo-sc-72aea6c3-9.fXjXhB > div.ProductAttributes-styles__Attributes-sc-303d66c3-1.dIfGXO.ProductDetailsSticky-styles__StyledProductAttributes-sc-72aea6c3-10.kJTavv > p:nth-child(2)");
                        return conditionElement ? conditionElement.innerText : 'NULL';
                        ''')
                    title = driver.execute_script('''
                        var titleElement = document.querySelector("#main > div.styles__Layout-sc-170b29e9-2.jTEyFL > div.styles__ContentWrapper-sc-170b29e9-3.kuRFFw > div.ProductDetailsSticky-styles__Wrapper-sc-72aea6c3-0.lmrJfz.styles__StyledProductDetailsSticky-sc-170b29e9-14.cfvkxK > div.ProductDetailsSticky-styles__DesktopKeyProductInfo-sc-72aea6c3-9.fXjXhB > h1");
                        return titleElement ? titleElement.innerText : 'NULL';
                        ''')
                    size = driver.execute_script('''
                        var titleElement = document.querySelector("#main > div.styles__Layout-sc-170b29e9-2.jTEyFL > div.styles__ContentWrapper-sc-170b29e9-3.kuRFFw > div.ProductDetailsSticky-styles__Wrapper-sc-72aea6c3-0.lmrJfz.styles__StyledProductDetailsSticky-sc-170b29e9-14.cfvkxK > div.ProductDetailsSticky-styles__DesktopKeyProductInfo-sc-72aea6c3-9.fXjXhB > div.ProductAttributes-styles__Attributes-sc-303d66c3-1.dIfGXO.ProductDetailsSticky-styles__StyledProductAttributes-sc-72aea6c3-10.kJTavv > p:nth-child(1)");
                        return titleElement ? titleElement.innerText : 'NULL';
                        ''')
                    driver.back()
                    visited_indices.add(idx)                     
                    unique_id = len(data) + 1
                    data.append({'ID': unique_id, 'Datetime': datetime_value, 'Link': href_value, 'Price': price, 'Description': description, 'Brand': brand, 'Condition': condition, 'Title': title, 'Size': size})
                except (NoSuchElementException):
                    print("Skipping element.")
                except StaleElementReferenceException:                        
                    print("Stale element. Refreshing.")
                    elements = driver.find_elements(By.CLASS_NAME, 'styles__ProductCard-sc-369aefb3-4')
                    break  
            except StaleElementReferenceException:
                print("Stale element. Refreshing.")
                elements = driver.find_elements(By.CLASS_NAME, 'styles__ProductCard-sc-369aefb3-4')
            break  

        if len(data) >= int(numValues):
            break

    current_scroll_position += window_height
    scroll_height = driver.execute_script("return document.documentElement.scrollHeight;")
    return data

def outputToFile(data):
    data = pd.DataFrame(data)
    data.to_json("data.json", orient='records', lines=True)
    print(f'Data has been saved')

def main():
    search_name_base = input("Enter search: ")
    num_values = input("Enter number of items to compare: ")
    searchname = search_name_base.replace(" ", "+")
    data = findValues(searchname, num_values)
    outputToFile(data)

main()