---
title: GRR 介面使用說明
author: Tony Lee
date: '2019-01-06'
slug: instruction_grr
categories: []
tags: []
---

### 輸入檔案格式：

輸入檔必須儲存為 csv 格式。檔案第一列為欄位名稱，數據從第二列開始。應包含四個欄位，分別代表 operator(操作人員代號), part(零件編號), trial(第幾次量測), value(量測值)。

<img src="/images/intro/grr_data.png" alt="使用介面" style="width:600px">


### 使用介面：

<img src="/images/intro/grr_submit.png" alt="使用介面" style="width:800px">

* 應用程式中含有內建的數據，使用者可以使用內建的數據測試及了解介面功能。使用內建數據，勾選 'Use demo data' 即可。如果要使用自己的數據，記得不要勾選這個選項。

* 選擇相對應的欄位。

* 若有公差，則輸入公差，GRR 計算才會帶出 % Tolerance 的值。

* 按 SUBMIT 執行。

* 執行會帶出 GRR 分析的結果。[解讀分析結果，參考連結](/post/grr_interpret)。

### 輸出報告

若要輸出 HTML 報告，在 'Generate GRR report' 的段落中輸入報告相關資料，再按 SUBMIT 即可產生報告。報告會開啟另一個網頁。

<img src="/images/intro/grr_report_submit.png" alt="使用介面" style="width:800px">
