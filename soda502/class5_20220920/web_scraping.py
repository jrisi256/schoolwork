from selenium import webdriver
from selenium.webdriver.firefox.options import Options

firefox_options = Options()
firefox_options.add_argument('--headless')
firefox_driver = webdriver.Firefox(executable_path = '/usr/local/bin/geckodriver', options = firefox_options)