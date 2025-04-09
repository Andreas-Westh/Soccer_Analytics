from playwright.sync_api import sync_playwright
from bs4 import BeautifulSoup
import pandas as pd

with sync_playwright() as p:
    browser = p.chromium.launch(headless=True)
    page = browser.new_page()
    response = page.goto("https://www.flashscore.dk/fodbold/danmark/superliga-2023-2024/tabeloversigt/#/2XDiHi8l/table/overall")
    #response = page.goto("https://www.flashscore.dk/fodbold/danmark/superliga-2022-2023/tabeloversigt/#/2XDiHi8l/table/overall")
    print(response.status)
    page.wait_for_selector("div.ui-table__body")  # mere præcis selector
    html = page.content()
    browser.close()



soup = BeautifulSoup(html, "html.parser")

rows = soup.select("div.ui-table__body div.ui-table__row")

data = []

for row in rows:
    try:
        rank = row.select_one("div.table__cell--rank div.tableCellRank").text.strip().replace(".", "")
        team = row.select_one("a.tableCellParticipant__name").text.strip()
        points = row.select_one("span.table__cell--points").text.strip()
        data.append({"Placering": int(rank), "Hold": team, "Point": int(points)})
    except Exception as e:
        print("Fejl:", e)
        continue
    
df = pd.DataFrame(data)
print(df)

df.to_csv("xG/Scraped_Data/Team_Rankings_2324.csv", index=False)
#df.to_csv("xG/Scraped_Data/Team_Rankings_2223.csv", index=False)
