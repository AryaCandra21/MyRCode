#load sebagian library
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(dashboardthemes)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(readxl)
library(leaflet)
library(shinycssloaders)
library(chorddiag)
library(tidyr)
library(DT)

#data yang diperlukan load dulu
dataNilaiMigas<-read_excel('NilaiEksporImporMigas.xlsx')
EksporMinyakMentah<-read_excel('EksporMinyakMentah.xlsx')
EksporHasilMinyak<-read_excel('EksporHasilMinyak.xlsx')
EksporGas<-read_excel('EksporGas.xlsx')
ImporMinyakMentah<-read_excel('ImporMinyakMentah.xlsx')

dataEksporImporAsean<-read_excel('EksporImporASEAN.xlsx')

dataNilaiNonMigas<-read_excel('NilaiEksporImporNonMigas.xlsx')
NilaiEksporNonMigasNegara<-read_excel('EksporNonMigasNegara.xlsx')
NilaiImporNonMigasNegara<-read_excel('ImporNonMigasNegara.xlsx')
imporNonMigasBarangModal<-read_excel('ImporNonMigasBarangModal.xlsx')
imporNonMigasBarangKonsumsi<-read_excel('ImporNonMigasBarangKonsumsi.xlsx')
imporNonMigasBahanPenolong<-read_excel('ImporNonMigasBahanBakuPenolong.xlsx')


#centroid peta
centroid<-read_excel('centroid.xlsx')
centroidIndonesia<-centroid[centroid$negara=='Indonesia', ]


###UI
ui<-bs4DashPage(freshTheme = 'modern',
  header = bs4DashNavbar(sidebarIcon = icon('list'),
                         title = 'Ekspor Import Migas',
                         status = 'info'),
                  skin = 'green',dark = NULL,
  sidebar=bs4DashSidebar(id = 'SidebarMenu',
                         vertical = T,status = 'info',skin = 'dark',
    br(),
    bs4SidebarHeader(title = strong('Menu')),
    bs4SidebarMenu(
      bs4SidebarMenuItem('Home', tabName = 'home', icon=icon('home')),
      bs4SidebarMenuItem('Visualisasi', icon = icon('chart-bar'),
                         bs4SidebarMenuSubItem('Migas', tabName = 'migas', icon=icon('gas-pump')),
                         bs4SidebarMenuSubItem('NonMigas', tabName = 'nonmigas', icon=icon('industry'))
                         ),
      bs4SidebarMenuItem('Data', tabName = 'data', icon=icon('database')),
      bs4SidebarMenuItem('Thank You', tabName = 'ThankYou',
                         icon=icon('hand-holding-heart'))
    ),
  ),
  body=bs4DashBody(
    #buat panel navigasi
    bs4TabItems(
      
      #panel home
      bs4TabItem(tabName = 'home',
               h1('Selamat Datang !',
                  style='font-size:300%'),
               column(width = 8, offset = 2,
                      br(),
                      br(),
                      box(width = 8,
                          title = strong('Did you know?'),
                          collapsed = T,icon = icon('info-circle'),
                          status = 'info', background = 'info',
                          solidHeader = T,
                      br(),
               p('Pada Agustus 2021, ekspor Indonesia mencatat rekor
                 nilai tertinggi sepanjang sejarah dengan surplus
                 sebesar', strong('US$4,75 miliar!'),
                  style='color:black;
                         background-color:aquamarine;
                         border-radius:10px;
                         font-size:120%;
                         text-align:center;
                         line-height:45px;
                         border: 2px solid green'))),
               sidebarLayout(
                 sidebarPanel(width = 8,
                  box(width = 12,title = strong('Migas'),
                      collapsible = F, icon = icon('gas-pump'),
                      status = 'info',
                      solidHeader = T,
                    p('Kalian tau gak sih, kalau rata-rata persentase nilai
                      ekspor migas',strong('hanya 8%'),
                      'dan rata-rata persentase impor
                      migas mencapai',strong('13,6%'),'?',
                      style='color:black;
                             font-size:120%;
                             text-align:justify'),
                    p('Sebelumnya, udah tau belum, apa itu migas?
                      Migas sendiri adalah singkatan dari minyak dan gas.
                      Barang migas pada dasarnya adalah produk mentah atau
                      olahan dari minyak bumi dan gas alam.',
                      style='color:black;
                             font-size:120%;
                             text-align:justify'),
                    p('Sayangnya, sejak tahun 2004,Indonesia sudah
                      menjadi net oil importer alias impor minyak Indonesia
                      lebih besar dari ekspor minyaknya.
                      Saat ini kita hanya mampu memproduksi',strong('800 ribu barel'),
                      'minyak per harinya, sedangkan kebutuhan minyak Indonesia
                      mencapai',strong('1,6 juta barel'),'per hari. Itulah kenapa, nilai
                      impor migas Indonesia lebih besar daripada nilai ekspor
                      migas Indonesia.',
                      style='color:black;
                             font-size:120%;
                             text-align:justify'),
                    p('Selain itu, cadangan migas kita
                      diperkirakan akan habis tidak lama lagi apabila tidak
                      ditemukan cadangan migas baru dan konsumsi migas masih
                      sangat tinggi seperti sekarang. Cadangan gas diprediksi
                      akan habis',strong('37,8 tahun lagi'),'bahkan lebih parahnya, cadangan
                      minyak akan habis dalam waktu',strong('12 tahun lagi'),'loh!',
                      style='color:black;
                             font-size:120%;
                             text-align:justify'),
                    p('Tapi jangan sedih, secara geologis, potensi migas kita masih
                      sangat menjanjikan kok bila kegiatan eksplorasi digalakkan!',
                      style='color:black;
                             font-size:120%;
                             text-align:justify')
                  )),
                 mainPanel(width = 4,
                           tags$img(src='FotoMigas.jpg',
                                    width='400px',
                                    height='400px',
                                    style='border-radius: 20px'))
                 ),
              br(),
              br(),
               sidebarLayout(position = 'right',
                 sidebarPanel(width = 8,
                  box(width = 12,
                      title = strong('Non Migas',
                                     style='text-align:right'),
                      collapsible = F,
                      status = 'info',
                      solidHeader = T,
                      icon = icon('industry'),
                    p('Lalu, kalian udah pernah denger belum, kalau',
                      strong('10 komoditas
                      terbesar'),'ekspor dan impor Indonesia semuanya adalah barang
                      nonmigas?',
                      style='color:black;
                             font-size:120%;
                             text-align:justify'),
                    p('Tapi, apa sih barang nonmigas itu? Jadi, barang nonmigas
                      adalah segala sesuatu yang merupakan hasil alam maupun
                      industri, tetapi bukan termasuk kategori minyak bumi dan
                      gas alam, seperti furnitur, hasil perkebunan, produk
                      farmasi, mesin pabrik, dan sebagainya.',
                      style='color:black;
                             font-size:120%;
                             text-align:justify'),
                   p('Kita semua sering banget nih, denger berita tentang
                     kebun sawit Indonesia yang kontroversial karena membuka
                     hutan dan mengurangi daerah resapan air dan juga
                     menghilangkan paru-paru dunia. Nah tetapi, ternyata nih ya,
                     dengan damage yang begitu besar, banyak kalangan yang masih
                     meragukan apakah Indonesia mampu mengoptimalkan daya
                     saingnya untuk memperoleh nilai tambah yang maksimal bagi
                     pembangunan ekonomi nasional. Hal ini karena sebagian besar
                     produk kelapa sawit Indonesia masih diperdagangkan dalam
                     bentuk CPO',em('(crude palm oil)'),'atau minyak goreng, dan belum
                     masuk ke tahap industry yang mempunyai nilai tambah besar
                     seperti industri biosurfaktan.',
                     style='color:black;
                            font-size:120%;
                            text-align:justify')
                   )),
                   mainPanel(width = 4,
                             column(width = 4,
                               tags$img(src='FotoNonMigas.jpg',
                                      width='400px',
                                      height='400px',
                                      style='border-radius: 20px')))
               ),
              br(),
              tags$a(href='https://www.instagram.com/belajarstatistics/',
                     tags$image(src='LogoBelajarStatistics.png',
                        width='100px',
                        height='100px',
                        style='display:block;
                               margin-left:auto;
                               margin-right:auto'),
              h5(strong('Disponsori : belajar statistics'),
                 style='text-align:center;
                        font-size:120%'),
      )),
      
      #panel migas
      bs4TabItem(tabName = 'migas',
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   box(title = 'Pengaturan Plot',
                       width = 12,icon = icon('tools'),
                       status = 'teal',solidHeader = T,
                     h3(strong('Atur Plotnya'),
                      style='color:blue'),
                     #pilihan jenis plot
                     radioButtons(inputId = 'jenisPlotMigas',
                                  label = 'Pilih jenis plot Migas',
                                  selected = 'Line plot',
                                  choices = c('Line plot',
                                              'Sankey plot',
                                              'Map plot',
                                              'Circos plot')),
    
                     #tampilkan UI hanya jika memilih map plot
                     uiOutput(outputId = 'UImapPlotMigas'),
                     
                     #tampilkan UI hanya jika memilih map plot
                     uiOutput(outputId = 'UItahunMapPlotMigas'),
                     
                     #tampilkan UI hanya jika memilih map plot
                     uiOutput(outputId = 'UIjenisMapPlotMigas'),
                     
                     #tampilkan UI hanya jika memilih map plot
                     uiOutput(outputId = 'UItahunSankeyPlotMigas'),
                     
                     #tampilkan UI hanya jika memilih line plot
                     uiOutput(outputId = 'UIindikatorLinePlotMigas'),
                     
                     #tampilkan UI hanya jika memilih line plot
                     uiOutput(outputId = 'UIlinePlotMigas'),
                     
                     #tampilkan UI hanya jika memilih Circos Plot
                     uiOutput(outputId = 'UItahunCircosPlotMigas'),
                     
                     #penunjuk plot
                     helpText(strong('Arahkan cursor ke plot agar
                                     lebih interaktif'))
                 )),
                 mainPanel(
                   br(),
                   uiOutput(outputId = 'UIpenjelasanMigas'),
                   #UI untuk melakukan plotting pada data migas
                   column(width=12, offset = 0,
                          br(),
                          br()),
                   uiOutput(outputId = 'UIplotMigas'),
                   br()
                 )
               ),
               fluidRow(
                column(
                  width = 12,
                  br(),
                  br(),
                  uiOutput(outputId = 'UIinterpretasiMigas',
                           inline = T),
                  br()
                )
              )
      ),
      
      #panel nonmigas
      bs4TabItem(tabName = 'nonmigas',
               #buat layout menjadi kiri dan kanan
               sidebarLayout(
                 #sidebar yang kiri
                 sidebarPanel(
                   width = 3,
                   box(title = 'Pengaturan Plot',
                       width = 12,icon = icon('tools'),
                       status = 'teal', solidHeader = T,
                     h3(strong('Atur Plotnya'),
                      style='color:blue'),
                     #buat pengaturan pilihan plot
                     radioButtons(inputId = 'jenisPlotNonMigas',
                                  label = 'Pilih jenis plot nonmigas',
                                  selected = 'Line plot',
                                  choices = c('Line plot',
                                              'Sunburst plot',
                                              'Map plot')),
                     
                     #tampilkan UI hanya jika memilih Line Plot
                     uiOutput(outputId = 'UIlinePlotNonMigas'),
                     
                     #tampilkan UI hanya jika memilih Map plot
                     uiOutput(outputId = 'UItahunMapPlotNonMigas'),
                     
                     #tampilkan UI hanya jika memilih Map plot
                     uiOutput(outputId = 'UIjenisMapPlotNonMigas'),
                     
                     #tampilkan UI hanya jika memilih sunburst plot
                     uiOutput(outputId = 'UItahunSunburstPlotNonMigas'),
                     
                     #penunjuk plot
                     helpText(strong('Arahkan cursor ke plot agar lebih interaktif'))
                 )),
                 #panel utama
                 mainPanel(
                   br(),
                   #UI untuk melakukan plotting pada data NonMigas
                   uiOutput(outputId = 'UIplotNonMigas'),
                   br()
                 )
               ),
               fluidRow(
                 column(
                   width = 12,
                   br(),
                   uiOutput(outputId = 'UIinterpretasiNonMigas'),
                   br()
                 )
               )
      ),
      
      #item yang data
      bs4TabItem(tabName = 'data',
        #buat layout menjadi kiri dan kanan
        sidebarLayout(
            sidebarPanel(
              width = 4,
              box(title = 'Pengaturan Tabel',
                  width = 12,icon = icon('tools'),
                  status = 'teal', solidHeader = T,
                  h4(strong('Atur Tabelnya'),
                 style='color:blue'),
              selectInput(inputId = 'cariData',
                          label = 'Pilih Data',
                          choices = c('migas',
                                      'nonmigas')),
              selectInput(inputId = 'kegiatanData',
                           label = 'Pilih ekspor atau impor',
                           choices = c('ekspor',
                                       'impor')),
              uiOutput(outputId = 'UIjenisData'),
              uiOutput(outputId = 'UIacuanData'),
              numericInput(inputId = 'banyakBaris',
                           label = 'Ingin berapa baris?',
                           value = 10,
                           min=1,
                           step = 1))
          ),
          mainPanel(
            tableOutput(outputId = 'UItabelInterest')
          )
        )
      ),
      
      #item yang about us
      bs4TabItem(tabName = 'ThankYou',
                 box(collapsible = F,width = 12,
                     title = strong('Terima Kasih, Pak'),icon = icon('gift'),
                   p(em('Voila!'), 'Setelah kurang lebih dua bulan bergelut dengan
                     lebih dari 2000 baris coding R shiny dan juga puluhan
                     website penyedia data ekspor dan impor, dengan bangga
                     kami persembahkan:'),
                   p('NAMA WEB,',
                      em('your source of import and export data!'),
                     style='text-align:center'),
                   p('Puji syukur kami panjatkan kepada Tuhan Yang Maha Esa,
                     atas limpahan berkah dan karunianya, kami dapat
                     menyelesaikan website R shiny untuk tugas akhir mata
                     kuliah Komputasi Statistik di semester 3 ini dengan
                     tepat waktu dan sebaik-baiknya.'),
                   p('Tugas R shiny ini kami persembahkan untuk dosen kami
                     yang paling kreatif dan penuh ide, ',
                     strong('Bapak Robert Kurniawan,
                     S.ST., M.Si.'), 'yang telah dengan penuh kesabaran dan
                     semangat senantiasa membimbing dan mendorong kami untuk
                     terus belajar dan mengeksplor segala hal di mata kuliah
                     Komputasi Statistik, khususnya pada materi R shiny.'),
                   p('Selain itu, kami juga mengucapkan terima kasih
                     sebesar-besarnya kepada teman-teman', em('TwoStoners'),
                     'yang selalu memberikan bantuan moral serta siap menjadi
                     tempat berkeluh kesah tentang betapa luar biasanya tugas
                     R shiny ini. Terima kasih untuk kalian yang secara langsung
                     ataupun tidak langsung telah menjadi inspirasi kami untuk terus
                     menghasilkan website yang lebih baik. '),
                   p('Kami menyadari bahwa website ini masih jauh dari kata
                     sempurna, sehingga kritik dan saran yang bersifat
                     membangun sangat kami harapkan agar kedepannya website
                     ini dan developer dapat menjadi lebih baik.'),
                   footer=p('Sincerely,',em('Arya & Aqila'),
                            style='text-align:right')
                 ))
    )
  )
)

###server
server<-function(input, output){
  
  #surpress error
  options(shiny.sanitize.errors=TRUE)
  
  #penjelasan Migas
  penjelasanMinyakMentah<-box(width = 12,title = strong('Info',
                                                        style='font-size:120%'),
                              icon = icon('info-circle'),
                              status = 'info', solidHeader = T,
                              background = 'info',height = 250,
    column(width=12, offset = 1.75,
    column(width = 10, offset = 1,
           column(width = 10,
                  offset = 5,
                  strong('Minyak Mentah')),
                  p('Minyak mentah adalah bahan bakar fosil yang terdapat
                     di bumi dan terbentuk dari tumbuhan dan hewan fosil
                     selama jutaan tahun. Minyak mentah disuling menjadi
                     berbagai produk minyak bumi, yang paling umum di
                     antaranya adalah bensin, atau gasolin.',
                     style='text-align:justify'),
                  style='color:black;
                         background-color:azure;
                         border-radius:10px;
                         border:2px solid indigo;
                         font-size:120%;
                         text-align:justify'),
    br(style='line-height:140px')
  ))
  
  penjelasanHasilMinyak<-box(width = 12,title = strong('Info',
                                                       style='font-size:120%'),
                             icon = icon('info-circle'),
                             status = 'info', solidHeader = T,
                             background = 'info',height = 250,
    column(width=12,offset = 1.75,
    column(width=10, offset = 1,
           column(width = 10,
                  offset = 5,
                  strong('Hasil Minyak')),
                  p('Hasil/produk minyak adalah bahan bermanfaat
                     (seperti bensin/gasolin yang berasal dari minyak
                     mentah (minyak bumi) setelah diproses/disuling
                     di pengolahan minyak.',
                     style='text-align:justify'),
                  style='color:black;
                         background-color:azure;
                         border-radius:10px;
                         border:2px solid indigo;
                         font-size:120%;
                         text-align:justify'),
    br(style='line-height:140px')
  ))
  
  penjelasanGas<-box(width = 12,title = strong('Info',
                                               style='font-size:120%'),
                     icon = icon('info-circle'),
                     status = 'info', solidHeader = T,
                     background = 'info',height = 250,
    column(width = 12, offset = 1.75,
    column(width=10, offset = 1,
           column(width=10,
                  offset = 5,
                  strong('Gas')),
                  p('Gas alam sering juga disebut sebagai gas bumi,
                     adalah bahan bakar fosil berbentuk gas yang terutama
                     terdiri dari metana. Gas alam adalah gas yang terkumpul
                     di bawah tanah dengan beragam komposisi dan merupakan
                     campuran hidrokarbon yang memiliki daya tekan tinggi dan
                     daya kembang besar serta secara alamiah dalam bentuk gas',
                     style='text-align:justify'),
                  style='color:black;
                         background-color:azure;
                         border-radius:10px;
                         border:2px solid indigo;
                         font-size:120%;
                         text-align:justify'),
    br(style='line-height:140px')
  ))
  
  output$UIpenjelasanMigas<-renderUI({
    if (input$jenisPlotMigas=='Line plot'){
      req(input$kategoriMigasGaris)
      switch(input$kategoriMigasGaris,
             'Minyak mentah'=penjelasanMinyakMentah,
             'Hasil minyak'=penjelasanHasilMinyak,
             'Gas'=penjelasanGas)
    }else if (input$jenisPlotMigas=='Map plot'){
      req(input$kategoriMigasPeta)
      switch(input$kategoriMigasPeta,
             'Minyak mentah'=penjelasanMinyakMentah,
             'Hasil minyak'=penjelasanHasilMinyak,
             'Gas'=penjelasanGas)
    }
  })
  
  #jika memilih Map Plot maka render UI pilihan kategori
  output$UImapPlotMigas<-renderUI({
    if (input$jenisPlotMigas=='Map plot'){
    #pilihan kategori migas
    selectInput(inputId = 'kategoriMigasPeta',
                label = 'Pilih kategori migas',
                choices = c('Minyak mentah',
                            'Hasil minyak',
                            'Gas'))
    }
  })
  
  #jika memilih Map Plot maka render UI pilihan tahun
  output$UItahunMapPlotMigas<-renderUI({
    if (input$jenisPlotMigas=='Map plot'){
      selectInput(inputId = 'tahunMapPlot',
                  label = 'Pilih tahun untuk peta',
                  selected = '2020',
                  choices = c('2016','2017','2018',
                              '2019','2020'))
    }
  })
 
  #jika memilih map plot maka render UI pilihan jenis plot
  output$UIjenisMapPlotMigas<-renderUI({
    if (input$jenisPlotMigas=='Map plot'){
      radioButtons(inputId = 'jenisMapPlotMigas',
                   label = 'Pilih Ekspor atau Impor',
                   choices = c('ekspor', 'impor'),
                   selected = 'ekspor')
    }
  })
  
  #Jika memilih Line Plot maka render Ui pilihan garisnya
  output$UIlinePlotMigas<-renderUI({
    if (input$jenisPlotMigas=='Line plot'){
      checkboxGroupInput(inputId = 'jenisLinePlotMigas',
                         label = 'Pilih garis',
                         choices = c('ekspor','impor'),
                         selected = c('ekspor','impor'))
      
    }
  })
  
  #render UI indikator line plot
  output$UIindikatorLinePlotMigas<-renderUI({
    if (input$jenisPlotMigas=='Line plot'){
      #pilihan kategori migas
      selectInput(inputId = 'kategoriMigasGaris',
                  label = 'Pilih kategori migas',
                  choices = c('Minyak mentah',
                              'Hasil minyak',
                              'Gas'))
    }
  })
  
  #render UI pilihan tahun untuk sankey plot
  output$UItahunSankeyPlotMigas<-renderUI({
    if (input$jenisPlotMigas=='Sankey plot'){
      selectInput(inputId = 'tahunSankeyPlotMigas',
                  label = 'Pilih tahun untuk sankey plot',
                  choices = c('2016','2017','2018',
                              '2019','2020'),
                  selected = '2019')
    }
  })
  
  #render UI pilihan tahun untuk circos plot
  output$UItahunCircosPlotMigas<-renderUI({
    if(input$jenisPlotMigas=='Circos plot'){
      selectInput(inputId = 'tahunCircosPlotMigas',
                  label = 'Pilih tahun untuk Circos plot',
                  choices = seq(2016,2019,1),
                  selected = 2019)
    }
  })
  
  #karena menggunakan leaflet dan plotly, maka ui perlu dirender lagi
  output$UIplotMigas<-renderUI({
    
    #jika memilih Map Plot banyak data yang belum ada
    if (input$jenisPlotMigas == 'Map plot'){
      
      #harus input jenisMapPlot dahulu
      req(input$jenisMapPlotMigas)
      
      #jika memilih impor
      if (input$jenisMapPlotMigas=='impor'){
        
        #harus input tahun dahulu
        req(input$tahunMapPlot)
        
        #yang ada datanya cuma Minyak Mentah 2018-2019
        if (input$kategoriMigasPeta=='Minyak mentah' &&
            input$tahunMapPlot %in% c('2018','2019')){
          leafletOutput(outputId = 'mapMigas',
                        height = 500,
                        width = 900) %>%
            withSpinner(image = 'loading.gif')
          
        #selain itu tidak ada datanya untuk impor
        }else{
          tags$img(src='DataNotFound.png')
        }
        
      #kalau milihnya ekspor datanya ada semua
      }else{
        leafletOutput(outputId = 'mapMigas',
                      height = 500,
                      width = 900) %>%
        withSpinner(image = 'loading.gif')
      }
      
    #berarti bukan milih map plot
    }else if (input$jenisPlotMigas=='Circos plot'){
      column(width=10,
        column(width = 10, offset = 3,
               h3('Plot Chord Diagram Ekspor Impor ASEAN tahun',
                  input$tahunCircosPlotMigas)),
      chorddiagOutput(outputId = 'circosMigas',
                      height = 800,
                      width = 900) %>%
        withSpinner(image = 'loading.gif')
      )
    }else{
      #kita handle yang sankey tapi tahun 2020
      if (input$jenisPlotMigas == 'Sankey plot'){
        #harus input tahun dahulu
        req(input$tahunSankeyPlotMigas)
        
        #jika tahunnya 2020, data belum tersedia
        if (input$tahunSankeyPlotMigas == '2020'){
          tags$img(src='DataNotFound.png')
        
        #selain itu data sudah ada
        }else{
          plotlyOutput(outputId = 'plotMigas',
                       height = 500,
                       width = 900,
                       inline = T) %>%
            withSpinner(image = 'loading.gif')
        }
      
      #berarti tidak memilih sankey plot
      }else{
        plotlyOutput(outputId = 'plotMigas',
                     height = 500,
                     width = 900,
                     inline = T) %>%
          withSpinner(image = 'loading.gif')
      }
    }
  })
  
  #render plot leaflet map plot
  output$mapMigas<-renderLeaflet({
    
    #buat kanvas peta kosong
    m<-leaflet()
    m<-addProviderTiles(m, providers$Esri.WorldStreetMap)
    
    if (input$jenisMapPlotMigas=='ekspor'){
      dataInterest<-switch(input$kategoriMigasPeta,
                          'Minyak mentah'=EksporMinyakMentah,
                          'Hasil minyak'=EksporHasilMinyak,
                          'Gas'=EksporGas)
    }else{
      dataInterest<-switch(input$kategoriMigasPeta,
                           'Minyak mentah'=ImporMinyakMentah,
                           'Hasil minyak'=ImporHasilMinyak,
                           'Gas'=ImporGas)
    }
  
    #untuk setiap baris pada data ekspor wilayah, lakukan penambahan garis
    for (i in 1:length(dataInterest$negara)){
      #buat variabel agar mudah
      kolomInterest<-paste('Nilai',
                            input$tahunMapPlot,
                            sep='')
      #ambil kolom
      negaraEkspor<-dataInterest[i,c('negara',
                                      'meanLat',
                                      'meanLong',
                                      kolomInterest)]
        
      #lakukan rbind negara ekspor dengan centroid
      centroidIndonesia[kolomInterest]<-negaraEkspor[kolomInterest]
      negaraEkspor<-rbind(negaraEkspor, centroidIndonesia)
        
      #jika tidak nol tambahkan garis
      if (sum(negaraEkspor[[kolomInterest]])!=0){
        m<-addPolylines(m, data=negaraEkspor,
                        lng = ~meanLong, lat= ~meanLat,
                        #buat label untuk setiap garis
                        label = paste(negaraEkspor$negara[1],
                                      'sebesar',
                                      negaraEkspor[kolomInterest][[1]],
                                      'juta USD'),
                        #ganti warna untuk setiap kategori agar bagus
                        color = switch(input$kategoriMigasPeta,
                                       'Minyak mentah'='tomato',
                                       'Hasil minyak'='green',
                                       'Gas'='red'))
      }
    }
    
    #tambahkan judul
    m<-addControl(m,strong(paste('Nilai',
                                 input$jenisMapPlotMigas,
                                 input$kategoriMigasPeta,
                                 input$tahunMapPlot,
                                 sep = ' ')),
                  position = 'topright')
    
    #spesifikasi zoom agar langsung terpusat di Indonesia
    m<-setView(m, lng=centroidIndonesia$meanLong,
               lat = centroidIndonesia$meanLat,
               zoom = 4.2)
    
    #setelah digunakan variabelnya diremove
    rm(list = c('dataInterest','kolomInterest','negaraEkspor'))
    
    return(m)
  })
  
  #code untuk plot selain peta pada Migas
  output$plotMigas<-renderPlotly({
    #jika dipilih line plot
    if (input$jenisPlotMigas=='Line plot'){
      #pengunjung web harus mengisi dulu jenisLinePlotnya kalau nggak error
      #maka, surpress dengan req
      req(input$jenisLinePlotMigas)
      
      #jika memilih indikator nilai
      fig<-ggplot()
      #pilihan kategori migas dibuat switch slice datanya saja
      dataInterest<-switch(input$kategoriMigasGaris,
                           #select yang ada hubungan dengan minyak mentah
                           'Minyak mentah'=select(dataNilaiMigas,
                                                  Tahun,
                                                  MinyakMentahEkspor,
                                                  MinyakMentahImpor,
                                                  frame),
                            #select yang ada hubungan dengan hasil minyak
                            'Hasil minyak'=select(dataNilaiMigas,
                                                  Tahun,
                                                  HasilMinyakEkspor,
                                                  HasilMinyakImpor,
                                                  frame),
                            #select yang ada hubungan dengan gas
                            'Gas'=select(dataNilaiMigas,
                                         Tahun,
                                         GasEkspor,
                                         GasImpor,
                                         frame))
      #buat reactive untuk menjadi penanda garis
      jenisLinePlotMigas<-reactive({
        if(length(input$jenisLinePlotMigas)==2){
          return('semua')
        }else{
          return(input$jenisLinePlotMigas)
        }
      })
        
      #buat dulu dasar plotlynya
      fig<-plot_ly(data=dataInterest,
                   type='scatter',
                   showlegend=F)
      #saya tidak menemukan cara selain menggunakan if else
      #karena kita harus passing non-string object ke parameter add_trace.
      # jika minyak mentah
      if (input$kategoriMigasGaris=='Minyak mentah'){
          
        #buat if untuk menambahkan layer garisnya
        if (jenisLinePlotMigas() %in% c('ekspor','semua')){
          fig<-add_trace(fig, x=~Tahun, y= ~MinyakMentahEkspor, frame=~frame,
                         #atur warna garis menjadi hijau (ekspor)
                         line=list(color='rgb(0,255,0)', width=3),
                         #atur warna marker menjadi hijau (ekspor)
                         marker=list(color='rgb(0,255,0)'),
                         type='scatter', name='ekspor', mode='lines+markers',
                         #template untuk tooltip
                         hovertemplate=paste('Tahun: %{x}\n',
                                             'Nilai: %{y} Juta USD',
                                             sep=''))
        }
        #tambahkan if lagi untuk menambahkan garis
        if (jenisLinePlotMigas() %in% c('impor','semua')){
          fig<-add_trace(fig, x=~Tahun, y= ~MinyakMentahImpor, frame=~frame,
                         #atur warna garis menjadi merah (impor)
                         line=list(color='rgb(255,0,0)', width=3),
                         #atur warna marker menjadi merah
                         marker=list(color='rgb(255,0,0)'),
                         type='scatter', name='impor', mode='lines+markers',
                         #template untuk tooltip
                         hovertemplate=paste('Tahun: %{x}\n',
                                             'Nilai: %{y} Juta USD',
                                             sep=''))
        }
      }else if (input$kategoriMigasGaris=='Hasil minyak'){
        #copy paste code di atas dengan penggantian sedikit
        if (jenisLinePlotMigas() %in% c('ekspor','semua')){
          fig<-add_trace(fig, x=~Tahun, y= ~HasilMinyakEkspor, frame=~frame,
                         line=list(color='rgb(0,255,0)', width=3),
                         marker=list(color='rgb(0,255,0)'),
                         type='scatter', name='ekspor', mode='lines+markers',
                         #template untuk tooltip
                         hovertemplate=paste('Tahun: %{x}\n',
                                             'Nilai: %{y} Juta USD',
                                             sep=''))
        }
        if (jenisLinePlotMigas() %in% c('impor','semua')){
          fig<-add_trace(fig, x=~Tahun, y= ~HasilMinyakImpor, frame=~frame,
                         line=list(color='rgb(255,0,0)', width=3),
                         marker=list(color='rgb(255,0,0)'),
                         type='scatter', name='impor', mode='lines+markers',
                         #template untuk tooltip
                         hovertemplate=paste('Tahun: %{x}\n',
                                             'Nilai: %{y} Juta USD',
                                             sep=''))
        }
      }else if (input$kategoriMigasGaris=='Gas'){
        #copy paste code di atas dengan penggantian sedikit
        if (jenisLinePlotMigas() %in% c('ekspor','semua')){
          fig<-add_trace(fig, x=~Tahun, y= ~GasEkspor, frame=~frame,
                         line=list(color='rgb(0,255,0)', width=3),
                         marker=list(color='rgb(0,255,0)'),
                         type='scatter', name='ekspor', mode='lines+markers',
                         #template untuk tooltip
                         hovertemplate=paste('Tahun: %{x}\n',
                                             'Nilai: %{y} Juta USD',
                                             sep=''))
        }
        if (jenisLinePlotMigas() %in% c('impor','semua')){
          fig<-add_trace(fig, x=~Tahun, y= ~GasImpor, frame=~frame,
                         line=list(color='rgb(255,0,0)', width=3),
                         marker=list(color='rgb(255,0,0)'),
                         type='scatter', name='impor', mode='lines+markers',
                         #template untuk tooltip
                         hovertemplate=paste('Tahun: %{x}\n',
                                             'Nilai: %{y} Juta USD',
                                             sep=''))
        }
      }
      #tambahkan aksesoris
      fig<-layout(fig, title=paste('Nilai',
                                    switch(jenisLinePlotMigas(),
                                           'ekspor'='ekspor',
                                           'impor'='impor',
                                           'semua'='ekspor dan impor'),
                                   input$kategoriMigasGaris,
                                   'Indonesia',
                                   sep=' '),
                  font=list(family='Arial',
                            size=12,
                            color='black'),
                  yaxis=list(title='Dalam Juta USD'),
                  plot_bgcolor='transparent',
                  paper_bgcolor='transparent')
      #agar gambarnya tidak lama
      fig<-animation_opts(fig,frame = 300,
                          transition = 200,
                          easing = 'linear-in')
      #hapus variabel agar tidak kehabisan memory
      rm(list = c('dataInterest'))
      
      return(fig)
      
    }else if(input$jenisPlotMigas=='Sankey plot'){
      #pengunjung web harus memilih tahun dulu
      req(input$tahunSankeyPlotMigas)
      
      #agar mudah kita buat variabel nilai
      kolomInterest<-paste('Nilai',
                           input$tahunSankeyPlotMigas,
                           sep='')
      
      #agar mudah kita buat variabel hasil sum
      minyakMentah<-sum(EksporMinyakMentah[[kolomInterest]])
      hasilMinyak<-sum(EksporHasilMinyak[[kolomInterest]])
      gas<-sum(EksporGas[[kolomInterest]])
      
      #definisikan asal sankey
      asalSankey<-c(rep('Indonesia',3),
                    rep('MinyakMentah',
                        length(EksporMinyakMentah[[kolomInterest]])),
                    rep('HasilMinyak',
                        length(EksporHasilMinyak[[kolomInterest]])),
                    rep('Gas',
                        length(EksporGas[[kolomInterest]]))
      )
      
      #definisikan tujuan sankey
      tujuanSankey<-c('MinyakMentah',
                      'HasilMinyak',
                      'Gas',
                      EksporMinyakMentah$negara,
                      EksporHasilMinyak$negara,
                      EksporGas$negara)
      
      #definisikan tujuan sankey
      valueSankey<-c(minyakMentah,
                     hasilMinyak,
                     gas,
                     EksporMinyakMentah[[kolomInterest]],
                     EksporHasilMinyak[[kolomInterest]],
                     EksporGas[[kolomInterest]])
      
      #definisikan data frame sankey
      sankeyDf<-data.frame(
        asal=asalSankey,
        tujuan=tujuanSankey,
        value=valueSankey
      )
      
      #definisikan seluruh nodes
      seluruhNodes<-data.frame(
        nodes=unique(c(sankeyDf$asal,
                       sankeyDf$tujuan))
      )
      
      #buat match asal dan tujuan terhadap seluruh nodes
      sankeyDf$IdAsal<-match(sankeyDf$asal,seluruhNodes$nodes)-1
      sankeyDf$IdTujuan<-match(sankeyDf$tujuan, seluruhNodes$nodes)-1
      
      #pengaturan plot sankey
      fig<-plot_ly(type = 'sankey',
                   orientation='h',
                   #mengatur node (titik dari sankey) 
                   node=list(
                     label=seluruhNodes$nodes,
                     pad=15,
                     thickness=20,
                     line=list(
                       color="black",
                       width=0.5
                     )
                   ),
                   #link berisi pengaturan hubungan (link dari sankey)
                   link= list(
                     source = sankeyDf$IdAsal,
                     target = sankeyDf$IdTujuan,
                     value = sankeyDf$value,
                     color='#ABFF2E97'
                   )
      )
      
      #atur judul
      fig<-layout(fig,
                  title=paste('Sankey Plot Nilai Ekspor Migas Tahun',
                              input$tahunSankeyPlotMigas,
                              '(dalam juta USD)',
                              sep=' '),
                  font=list(family='Arial',
                            size=12,
                            color='black'),
                  plot_bgcolor='transparent',
                  paper_bgcolor='transparent')
      #hapus variabel agar tidak kehabisan memory
      rm(list = c('kolomInterest', 'minyakMentah','hasilMinyak',
                  'gas','asalSankey','tujuanSankey','valueSankey',
                  'sankeyDf','seluruhNodes'))
      return(fig)
    }
  })
  
  output$circosMigas<-renderChorddiag({
    #kita filter dulu datanya
    dataFilter<-dataEksporImporAsean[
      dataEksporImporAsean$Year==input$tahunCircosPlotMigas, ]
    
    #kita buat menjadi bentuk wider
    dataInterest<-pivot_wider(dataFilter,
                              id_cols = `Reporter Name`,
                              names_from = `Partner Name`,
                              values_from = Export)
    #kita ambil negaranya
    negara<-dataInterest$`Reporter Name`
    
    #hapus kolom pertama yang berisi reporter name
    dataInterest<-dataInterest[-1]
    
    #buat menjadi model matrix, yakni matriks persegi
    m<-data.matrix(dataInterest)
    #buat rownames matrixnya negara
    rownames(m)<-negara
    #kita sort
    m<-m[,negara]
    chorddiag(m,
              palette = 'Paired',
              groupnameFontsize = 14)
  })
  
  #kasih interpretasi untuk data Migas
  output$UIinterpretasiMigas<-renderUI({
    
    #jika memilih line plot
    if (input$jenisPlotMigas=='Line plot'){
      
      #jika memilih minyak mentah
      req(input$kategoriMigasGaris)
      if (input$kategoriMigasGaris=='Minyak mentah'){
        box(width = 12,title = strong('Interpretasi'),
            status = 'info', background = 'info',icon = icon('lightbulb'),
            solidHeader = T, gradient = T,
          column(width=12,
                 column(width = 8, offset = 2,
                        h4('Interpretasi Kondisi Minyak Mentah',
                           style='color:black;
                           text-align:center'),
                        style='background-color:lavender;
                               border-radius: 20px;
                               border:2px solid ForestGreen'),
                 column(width=10, offset = 0,
                        br()),
                 column(width = 10, offset = 1,
                        br(),
                        p('Kita dapat melihat bahwa pada tahun 1996-2008 kondisi
                          ekspor dan impor minyak mentah di Indonesia',
                          strong('cenderung meningkat'),
                          'Akan tetapi, pada tahun 2009 terlihat',
                          strong('terjadi penurunan'), 'baik pada ekspor maupun
                          impor minyak mentah.
                          Hal ini disebabkan oleh krisis finansial global yang
                          terjadi pada tahun tersebut.',
                          style='text-indent:30px;
                                 text-align:justify'),
                        p('Selain itu, pada periode tahun 1996-2012, nilai Ekspor
                          minyak mentah',
                          strong('selalu melebihi'),'nilai Impornya dan
                          cenderung meningkat. Puncaknya, pada tahun 2011,
                          Indonesia melakukan ekspor minyak mentah senilai
                          ',strong('13,8 Ribu Juta USD.'),
                           style='text-indent:30px;
                                  text-align:justify'),
                        p('Akan tetapi, trend ini berubah setelahnya. Mulai tahun
                          2013, kita dapat melihat bahwa ekspor minyak mentah
                          selalu lebih tinggi dari nilai impor. Ditambah lagi,
                          nilai dari ekspor dan impor minyak mentah Indonesia',
                          strong('cenderung turun'), 'tiap tahunnya.',
                           style='text-indent:30px;
                                  text-align:justify'),
                 style='background-color:lavender;
                        border-radius: 20px;
                        align:justify;
                        color:black;
                        border:2px solid ForestGreen')
                 )
        )
        #jika memilih hasil minyak
      }else if (input$kategoriMigasGaris=='Hasil minyak'){
        box(width = 12,title = strong('Interpretasi'),
            status = 'info', background = 'info',icon = icon('lightbulb'),
            solidHeader = T, gradient = T,
          column(width=12,
                 column(width = 8, offset = 2,
                        h4('Interpretasi Kondisi Hasil minyak',
                           style='color:black;
                                  text-align:center'),
                        style='background-color:lavender;
                               border-radius: 20px;
                               border:2px solid ForestGreen'),
                 column(width=10, offset = 0,
                        br()),
                 column(width = 10, offset = 1,
                        br(),
                        p('Berdasarkan data yang ada, didapatkan bahwa Indonesia',
                          strong('cenderung melakukan impor'),'hasil minyak
                          dibandingkan
                          melakukan ekspor. Selisih antara Impor dan Ekspor ini
                          selama periode 1996-2014 cenderung semakin meningkat.
                          Di tahun 2009, kita juga dapat melihat terjadi sedikit
                          penurunan karena krisis finansial global.',
                          style='text-indent:30px;
                                 text-align:justify'),
                        p('Secara rata-rata, nilai ekspor Hasil Minyak pada
                          periode 1996-2020 berkisar pada',strong('2500 Ribu Juta USD'),
                          '. Terlihat juga bahwa mulai tahun 2014 cenderung terjadi
                          trend penurunan nilai impor Hasil Minyak di Indonesia.',
                          style='text-indent:30px;
                                text-align:justify'),
               style='background-color:lavender;
                      border-radius: 20px;
                      align:justify;
                      color:black;
                      border:2px solid ForestGreen')
          )
        )
        #jika memilih gas
      }else if (input$kategoriMigasGaris=='Gas'){
        box(width = 12,title = strong('Interpretasi'),
            status = 'info', background = 'info',icon = icon('lightbulb'),
            solidHeader = T, gradient = T,
          column(width=12,
                 column(width = 8, offset = 2,
                        h4('Interpretasi Kondisi Gas',
                           style='color:black;
                         text-align:center'),
                        style='background-color:lavender;
                               border-radius: 20px;
                               border:2px solid ForestGreen'),
                 column(width=10, offset = 0,
                        br()),
                 column(width = 10, offset = 1,
                        br(),
                        p('Dari plot tersebut, terlihat kondisi yang berkebalikan
                          dengan ekspor impor hasil minyak di Indonesia. Hal yang
                          dimaksud adalah nilai ekspor gas di Indonesia',
                          strong('selalu
                          lebih tinggi dari nilai impornya.'),
                          style='text-indent:30px;
                                 text-align:justify'),
                        p('Nilai ekspor gas pada periode 1996-2011 cenderung
                          meningkat nilainya. Puncak dari nilai ekspor Gas di
                          Indonesia terjadi pada tahun 2011, yakni sebesar',
                          strong('22.8 Ribu Juta USD'),'.Fenomena krisis finansial
                          global juga
                          tergambarkan pada kondisi ekspor gas, tetapi tidak
                          terlalu berpengaruh pada kondisi impornya. Setelah
                          trend ekspor yang meningkat pada periode 1996-2011,
                          terjadi trend penurunan nilai ekspor gas di Indonesia
                          mulai tahun 2011.',
                          style='text-indent:30px;
                                 text-align:justify'),
               style='background-color:lavender;
                      border-radius: 20px;
                      align:justify;
                      color:black;
                      border:2px solid ForestGreen')
          )
        )
      }
      
      #interpretasi jika map plot
    }else if (input$jenisPlotMigas=='Map plot'){
      #harus input kategoriMigasPeta dahulu
      req(input$kategoriMigasPeta)
      
      #jika meilih minyak mentah
      if (input$kategoriMigasPeta=='Minyak mentah'){
        
        #jika minyak mentahnya yang ekspor
        if (input$jenisMapPlotMigas=='ekspor'){
          box(width = 12,title = strong('Interpretasi'),
              status = 'info', background = 'info',icon = icon('lightbulb'),
              solidHeader = T, gradient = T,
            column(width=12,
                   column(width = 8, offset = 2,
                          h4('Interpretasi Map Plot Minyak Mentah',
                             style='color:black;
                           text-align:center'),
                          style='background-color:lavender;
                                 border-radius: 20px;
                                 border:2px solid ForestGreen'),
                   column(width=10, offset = 0,
                          br()),
                   column(width = 10, offset = 1,
                          br(),
                          p('Dari map plot ekspor migas minyak mentah,
                            terlihat bahwa mayoritas tujuan ekspor minyak
                            mentah Indonesia adalah negara-negara di',
                            strong('Benua Asia.'),
                            'Pada 2018, terdapat Singapura, Malaysia, Thailand, Cina,
                            Taiwan, Korea Selatan, Jepang, Amerika Serikat, dan Australia
                            sebagai negara pengimpor minyak mentah dari Indonesia dengan
                            nilai besar. Namun, pada 2019, Amerika Serikat tidak melakukan
                            impor minyak mentah dari Indonesia dengan nilai besar.
                            Klimaksnya pada tahun 2020, hanya tersisa enam dari sembilan
                            negara yang melakukan impor minyak mentah dengan nilai besar,
                            yaitu Singapura, Malaysia, Thailand, Cina, Jepang, dan Australia.',
                            style='text-indent:30px;
                                   text-align:justify'),
                          p('Selama periode 2018 - 2020,',
                            strong('Thailand'), 'selalu menjadi negara pengimpor
                            minyak mentah dari Indonesia dengan nilai terbesar. Pada tahun 2018,
                            nilai impornya ke Indonesia mencapai',
                            strong('977,7 juta USD'),'. Namun, nilainya
                            cenderung menurun dari tahun ke tahun. ',
                            style='text-indent:30px;
                                   text-align:justify'),
                          p('Begitupula dengan negara-negara lain yang juga mengalami penyusutan
                            nilai impor minyak mentah dari Indonesia. Hal ini disebabkan karena
                            pertumbuhan ekonomi dan perdagangan di negara tujuan utama ekspor
                            Indonesia mengalami penurunan, sehingga nilai impor mereka juga ikut
                            menurun.',
                            style='text-indent:30px;
                                   text-align:justify'),
                          style='background-color:lavender;
                        border-radius: 20px;
                        align:justify;
                        color:black;
                        border:2px solid ForestGreen')
            )
          )
          
          #jika milihnya yang minyak mentah import
        }else{
          box(width = 12,title = strong('Interpretasi'),
              status = 'info', background = 'info',icon = icon('lightbulb'),
              solidHeader = T, gradient = T,
            column(width=12,
                   column(width = 8, offset = 2,
                          h4('Interpretasi Map Plot Minyak Mentah',
                             style='color:black;
                           text-align:center'),
                          style='background-color:lavender;
                                 border-radius: 20px;
                                 border:2px solid ForestGreen'),
                   column(width=10, offset = 0,
                          br()),
                   column(width = 10, offset = 1,
                          br(),
                          p('Dari map plot impor migas minyak mentah,
                            terlihat bahwa mayoritas asal impor minyak mentah
                            Indonesia adalah negara-negara di Benua Afrika.
                            Negara yang konsisten melakukan impor gas dari Indonesia
                            dengan nilai besar pada periode 2018 - 2019 adalah Angola,
                            Nigeria, Algeria, Arab Saudi, dan Singapura.
                            Pada tahun 2018 - 2019, Arab Saudi berturut-turut menyumbang
                            nilai impor minyak mentah tertinggi yaitu 2.580,19 juta USD
                            dan 2.703,29 juta USD.',
                            style='text-indent:30px;
                                   text-align:justify'),
                          style='background-color:lavender;
                        border-radius: 20px;
                        align:justify;
                        color:black;
                        border:2px solid ForestGreen')
            )
          )
        }
        
        #jika memilih hasil minyak
      }else if (input$kategoriMigasPeta=='Hasil minyak'){
        box(width = 12,title = strong('Interpretasi'),
            status = 'info', background = 'info',icon = icon('lightbulb'),
            solidHeader = T, gradient = T,
          column(width=12,
                 column(width = 8, offset = 2,
                        h4('Interpretasi Map Plot Hasil Minyak',
                           style='color:black;
                         text-align:center'),
                        style='background-color:lavender;
                               border-radius: 20px;
                               border:2px solid ForestGreen'),
                 column(width=10, offset = 0,
                        br()),
                 column(width = 10, offset = 1,
                        br(),
                        p('Dari map plot ekspor migas hasil minyak,
                          terlihat bahwa negara tujuan ekspor hasil minyak
                          Indonesia didominasi oleh negara-negara di Benua Asia.
                          Terdapat Singapura, Malaysia, Thailand, Cina, Korea Selatan,
                          Jepang, Amerika Serikat, Australia, dan India sebagai negara
                          pengimpor hasil minyak dari Indonesia dengan nilai besar.
                          Kesembilan negara ini konsisten melakukan impor hasil minyak
                          dari Indonesia dengan nilai besar selama periode 2018 - 2020.',
                          style='text-indent:30px;
                                 text-align:justify'),
                        p('Selama periode 2018 - 2020, Malaysia selalu menjadi negara
                          pengekspor hasil minyak ke Indonesia dengan nilai terbesar.
                          Puncaknya terjadi pada tahun 2019, nilai impornya ke Indonesia
                          mencapai 893,2 juta USD. Nilai ini meningkat drastis dibandingkan
                          dengan tahun 2018 yang hanya mencapai 583,2 juta USD. Namun,
                          pada 2020, nilainya menurun sebesar 6,75% menjadi 832,9 juta USD. ',
                          style='text-indent:30px;
                                 text-align:justify'),
                        style='background-color:lavender;
                      border-radius: 20px;
                      align:justify;
                      color:black;
                      border:2px solid ForestGreen')
          )
        )
        
        #jika memilih gas
      }else if (input$kategoriMigasPeta=='Gas'){
        box(width = 12,title = strong('Interpretasi'),
            status = 'info', background = 'info',icon = icon('lightbulb'),
            solidHeader = T, gradient = T,
          column(width=12,
                 column(width = 8, offset = 2,
                        h4('Interpretasi Map Plot Migas Gas',
                           style='color:black;
                         text-align:center'),
                        style='background-color:lavender;
                               border-radius: 20px;
                               border:2px solid ForestGreen'),
                 column(width=10, offset = 0,
                        br()),
                 column(width = 10, offset = 1,
                        br(),
                        p('Dari map plot ekspor migas gas,
                          terlihat bahwa mayoritas tujuan ekspor gas Indonesia
                          adalah negara-negara di Benua Asia. Negara yang konsisten
                          melakukan impor gas dari Indonesia dengan nilai besar pada
                          periode 2018 - 2020 adalah Singapura, Malaysia, Thailand,
                          Cina, Korea Selatan, dan Jepang. Pada 2019, Filipina juga
                          melakukan impor gas dari Indonesia dan pada 2020, Australia
                          juga melakukan impor gas dari Indonesia.',
                          style='text-indent:30px;
                                 text-align:justify'),
                        p('Selama periode 2018 - 2020, Singapura selalu menjadi negara
                          pengimpor minyak mentah dari Indonesia dengan nilai terbesar.
                          Pada tahun 2018, nilai impornya ke Indonesia mencapai 3205,7
                          juta USD. Namun, nilainya cenderung menurun dari tahun ke tahun.
                          Pada tahun 2020, terjadi penurunan sebesar 46,9% dibandingkan
                          2018 menjadi hanya 1701,8 juta USD.',
                          style='text-indent:30px;
                                 text-align:justify'),
                        style='background-color:lavender;
                      border-radius: 20px;
                      align:justify;
                      color:black;
                      border:2px solid ForestGreen')
          )
        )
      }
      
      #interpretasi jika sankey plot
    }else if (input$jenisPlotMigas=='Sankey plot'){
      box(width = 12,title = strong('Interpretasi'),
          status = 'info', background = 'info',icon = icon('lightbulb'),
          solidHeader = T, gradient = T,
        column(width=12,
               column(width = 8, offset = 2,
                      h4('Interpretasi Sankey Plot Migas',
                         style='color:black;
                         text-align:center'),
                      style='background-color:lavender;
                               border-radius: 20px;
                               border:2px solid ForestGreen'),
               column(width=10, offset = 0,
                      br()),
               column(width = 10, offset = 1,
                      br(),
                      p('Selama tahun 2018 - 2020,
                        nilai kategori impor migas Indonesia didominasi oleh produk gas,
                        lalu diikuti oleh minyak mentah, dan yang terakhir hasil minyak.
                        Namun, pada tiga periode tahun ini, trend untuk  semua kategori
                        cenderung mengalami penurunan, kecuali pada tahun 2019 untuk
                        impor migas kategori hasil minyak mengalami peningkatan sebesar
                        0,16 juta USD. ',
                        style='text-indent:30px;
                                 text-align:justify'),
                      p('Di antara ketiga tahun tersebut,
                        tahun 2020 mengalami penurunan yang cukup signifikan dibandingkan
                        tahun-tahun sebelumnya. Hal ini disebabkan oleh pandemic Covid-19
                        yang cukup kuat memengaruhi perdagangan dunia, termasuk Indonesia.',
                        style='text-indent:30px;
                                 text-align:justify'),
                      p('Mayoritas negara yang merupakan negara asal impor migas Indonesia
                        adalah negara-negara di Benua Asia. Singapura konsisten menjadi negara
                        eksportir penyumbang nilai terbesar untuk impor migas Indonesia selama
                        tahun 2018 - 2020.',
                        style='text-indent:30px;
                                 text-align:justify'),
                      style='background-color:lavender;
                      border-radius: 20px;
                      align:justify;
                      color:black;
                      border:2px solid ForestGreen')
        )
      )
    }else if (input$jenisPlotMigas=='Circos plot'){
      box(width = 12,title = strong('Interpretasi'),
          status = 'info', background = 'info',icon = icon('lightbulb'),
          solidHeader = T, gradient = T,
        column(width=12,
               column(width = 8, offset = 2,
                      h4('Interpretasi Circos Plot Migas',
                         style='color:black;
                         text-align:center'),
                      style='background-color:lavender;
                               border-radius: 20px;
                               border:2px solid ForestGreen'),
               column(width=10, offset = 0,
                      br()),
               column(width = 10, offset = 1,
                      br(),
                      p('Dari circos plot di atas, terlihat
                        bahwa negara-negara di ASEAN aktif melakukan
                        perdagangan ekspor dan impor dengan sesama
                        negara ASEAN. Dari tahun 2016 - 2019, Singapura
                        selalu menjadi eksportir terbesar kepada negara-negara
                        ASEAN. Tujuan ekspor terbesar Singapura adalah Malaysia
                        dan Indonesia.',
                        style='text-indent:30px;
                                 text-align:justify'),
                      p('Pda tahun 2016 - 2018, nilai ekspor Singapura ke Malaysia
                        lebih besar dari nilai impor Singapura dari Malaysia.
                        Namun, pada 2019, nilai ekspor Malaysia ke Singapura melampaui
                        nilai impor Malaysia dari Singapura. Hal ini disebabkan oleh
                        penurunan ekspor Singapura secara keseluruhan akibat penurunan
                        siklus teknologi global dan pertumbuhan yang lebih lemah di Cina.
                        Selain itu, hal ini juga dipicu oleh kontraksi ekspor ke Amerika
                        Serikat dan juga penyusutan pengiriman ke Eropa.',
                        style='text-indent:30px;
                                 text-align:justify'),
                      p('Tidak hanya di Singapura, nilai dan volume ekspor di
                        tiap negara ASEAN rata-rata mengalami penurunan pada 2019.
                        Hal ini terjadi akibat risiko dari kelanjutan perang dagang
                        antara Amerika Serikat dan China yang merupakan dua negara
                        penguasa perdagangan dunia.',
                        style='text-indent:30px;
                                 text-align:justify'),
                      style='background-color:lavender;
                      border-radius: 20px;
                      align:justify;
                      color:black;
                      border:2px solid ForestGreen')
        )
      )
    }
  })
  
  ######batas untuk Migas dan Nonmigas
  
  #render UI plot NonMigas
  output$UIplotNonMigas<-renderUI({
    if (input$jenisPlotNonMigas=='Map plot'){
      leafletOutput(outputId = 'mapNonMigas',
                    height = 500,
                    width = 900) %>%
        withSpinner(image = 'loading.gif')
    }else{
      plotlyOutput(outputId = 'plotNonMigas',
                   height = 500,
                   width = 900,
                   inline = T) %>%
        withSpinner(image = 'loading.gif')
    }
  })
  
  #render UI untuk pilihan Line Plot
  output$UIlinePlotNonMigas<-renderUI({
    if (input$jenisPlotNonMigas=='Line plot'){
      checkboxGroupInput(inputId = 'jenisLinePlotNonMigas',
                    label = 'Pilih garis',
                    choices = c('ekspor','impor'),
                    selected = c('ekspor','impor'))
    }
  })
  
  #render UI untuk pilihan tahun map plot
  output$UItahunMapPlotNonMigas<-renderUI({
    if(input$jenisPlotNonMigas=='Map plot'){
      selectInput(inputId = 'tahunMapPlotNonMigas',
                  label = 'Pilih tahun',
                  choices = c('2018',
                              '2019',
                              '2020'),
                  selected = '2020')
    }
  })
  
  #render UI untuk pilihan jenis plot map plot
  output$UIjenisMapPlotNonMigas<-renderUI({
    if(input$jenisPlotNonMigas=='Map plot'){
      radioButtons(inputId = 'jenisMapPlotNonMigas',
                   label = 'Pilih ekspor atau impor',
                   choices = c('ekspor',
                               'impor'),
                   selected = 'ekspor')
    }
  })
  
  #render UI untuk pilihan tahun sunburst plot
  output$UItahunSunburstPlotNonMigas<-renderUI({
    if (input$jenisPlotNonMigas=='Sunburst plot'){
      selectInput(inputId = 'tahunSunburstPlotNonMigas',
                  label = 'Pilih tahun',
                  choices = c('2016',
                              '2017',
                              '2018',
                              '2019',
                              '2020'),
                  selected = '2020')
    }
  })
  
  #code untuk plot selain peta pada NonMigas
  output$plotNonMigas <- renderPlotly({
    
    #jika plotnya adalah Line plot
    if (input$jenisPlotNonMigas=='Line plot'){
      
      #pengunjung website harus mengisi dulu pilihan garis
      req(input$jenisLinePlotNonMigas)
      #load data yang ingin diplot
      dataInterest<-dataNilaiNonMigas
      
      #buat reactive untuk menjadi penanda garis
      jenisLinePlotNonMigas<-reactive({
        if(length(input$jenisLinePlotNonMigas)==2){
          return('semua')
        }else{
          return(input$jenisLinePlotNonMigas)
        }
      })
      #buat dulu dasar plotlynya
      fig<-plot_ly(data=dataInterest,
                   type='scatter',
                   showlegend=F)
      
      #buat if untuk menambahkan layer garisnya
      if (jenisLinePlotNonMigas() %in% c('ekspor','semua')){
        fig<-add_trace(fig, x=~Tahun, y= ~NonMigasEkspor, frame=~frame,
                       #atur warna garis menjadi hijau (ekspor)
                       line=list(color='rgb(0,255,0)', width=3),
                       #atur warna marker menjadi hijau (ekspor)
                       marker=list(color='rgb(0,255,0)'),
                       type='scatter', name='ekspor', mode='lines+markers',
                       #template untuk tooltip
                       hovertemplate=paste('Tahun: %{x}\n',
                                           'Nilai: %{y} Juta USD',
                                           sep=''))
      }
      #tambahkan if lagi untuk menambahkan garis
      if (jenisLinePlotNonMigas() %in% c('impor','semua')){
        fig<-add_trace(fig, x=~Tahun, y= ~NonMigasImpor, frame=~frame,
                       #atur warna garis menjadi merah (impor)
                       line=list(color='rgb(255,0,0)', width=3),
                       #atur warna marker menjadi merah
                       marker=list(color='rgb(255,0,0)'),
                       type='scatter', name='impor', mode='lines+markers',
                       #template untuk tooltip
                       hovertemplate=paste('Tahun: %{x}\n',
                                           'Nilai: %{y} Juta USD',
                                           sep=''))
      }
      
      #tambahkan aksesoris
      fig<-layout(fig, title=paste('Nilai',
                                   switch(jenisLinePlotNonMigas(),
                                          'ekspor'='ekspor',
                                          'impor'='impor',
                                          'semua'='ekspor dan impor'),
                                   'NonMigas Indonesia'),
                  font=list(family='Arial',
                            size=12,
                            color='black'),
                  yaxis=list(title='Dalam Juta USD'),
                  plot_bgcolor='transparent',
                  paper_bgcolor='transparent')
      
      #agar gambarnya tidak lama
      fig<-animation_opts(fig,frame = 300,
                          transition = 200,
                          easing = 'linear-in')
      #hapus variabel yang sudah digunakan
      rm(list = c('dataInterest'))
      
      return(fig)
      
    }else{
      
      #jika memilih plot Sunburst
      if (input$jenisPlotNonMigas=='Sunburst plot'){
        #harus input tahunnya dulu
        req(input$tahunSunburstPlotNonMigas)
      
        #buat variabel agar mudah
        kolomInterest<-paste('Nilai',
                             input$tahunSunburstPlotNonMigas,
                             sep='')
        #hitung beberapa kolom yang diinginkan
        jumlah_modal<-sum(imporNonMigasBarangModal[[kolomInterest]])
        jumlah_konsumsi<-sum(imporNonMigasBarangKonsumsi[[kolomInterest]])
        jumlah_penolong<-sum(imporNonMigasBahanPenolong[[kolomInterest]])
        
        #setelah itu buat kanvas plotly
        fig<-plot_ly(
          #buat labelsnya
          labels=c('Modal',
                   'Konsumsi',
                   'Penolong',
                   imporNonMigasBarangModal$Uraian,
                   imporNonMigasBarangKonsumsi$Uraian,
                   imporNonMigasBahanPenolong$Uraian),
          #buat parents untuk label
          parents=c('',
                    '',
                    '',
                    rep('Modal',
                        times=length(imporNonMigasBarangModal$Uraian)),
                    rep('Konsumsi',
                        times=length(imporNonMigasBarangKonsumsi$Uraian)),
                    rep('Penolong',
                        times=length(imporNonMigasBahanPenolong$Uraian))),
          #buat valuesnya
          values=c(jumlah_modal,
                   jumlah_konsumsi,
                   jumlah_penolong,
                   imporNonMigasBarangModal[[kolomInterest]],
                   imporNonMigasBarangKonsumsi[[kolomInterest]],
                   imporNonMigasBahanPenolong[[kolomInterest]]),
          branchvalues='total',
          insidetextorientation='radial',
          type='sunburst'
        )
        
        fig <- layout(fig, title='Nilai impor NonMigas menurut golongan (dalam Juta USD)',
                      font=list(family='Arial',
                                size=12,
                                color='black'),
                      plot_bgcolor='transparent',
                      paper_bgcolor='transparent')
        return(fig)
      }
    }
  })
  
  #peta untuk nonmigas
  output$mapNonMigas<-renderLeaflet({
    if (input$jenisPlotNonMigas=='Map plot'){
      #buat kanvas peta kosong
      m<-leaflet()
      m<-addProviderTiles(m, providers$Esri.WorldStreetMap)
      
      #buat variabel data interest agar mudah
      dataInterest<-switch(input$jenisMapPlotNonMigas,
                           'ekspor'=NilaiEksporNonMigasNegara,
                           'impor'=NilaiImporNonMigasNegara)
      
      #untuk setiap baris pada data ekspor wilayah, lakukan penambahan garis
      for (i in 1:length(dataInterest$negara)){
        #buat variabel agar mudah
        kolomInterest<-paste('Nilai',
                             input$tahunMapPlotNonMigas,
                             sep='')
        #ambil kolom
        negaraEkspor<-dataInterest[i,c('negara',
                                       'meanLat',
                                       'meanLong',
                                       kolomInterest)]
        
        #lakukan rbind negara ekspor dengan centroid
        centroidIndonesia[kolomInterest]<-negaraEkspor[kolomInterest]
        negaraEkspor<-rbind(negaraEkspor, centroidIndonesia)
        
        #jika tidak nol tambahkan garis
        if (sum(negaraEkspor[[kolomInterest]])!=0){
          m<-addPolylines(m, data=negaraEkspor,
                          lng = ~meanLong, lat= ~meanLat,
                          #buat label untuk setiap garis
                          label = paste(negaraEkspor$negara[1],
                                        'sebesar',
                                        negaraEkspor[kolomInterest][[1]],
                                        'juta USD'),
                          #ganti warna untuk setiap kategori agar bagus
                          color = switch(input$jenisMapPlotNonMigas,
                                         'ekspor'='green',
                                         'impor'='red'))
        }
      }
      
      #tambahkan judul
      m<-addControl(m,strong(paste('Nilai',
                                   input$jenisMapPlotNonMigas,
                                   'Non Migas Indonesia tahun',
                                   input$tahunMapPlotNonMigas,
                                   sep = ' ')),
                    position = 'topright')
      
      #spesifikasi zoom agar langsung terpusat di Indonesia
      m<-setView(m, lng=centroidIndonesia$meanLong,
                 lat = centroidIndonesia$meanLat,
                 zoom = 4.2)
      
      #setelah digunakan variabelnya diremove
      rm(list = c('dataInterest','kolomInterest','negaraEkspor'))
      
      return(m)
    }
  })
  
  #kasih interpretasi untuk NonMigas
  output$UIinterpretasiNonMigas<-renderUI({
    
    #jika memilih Line plot
    if (input$jenisPlotNonMigas=='Line plot'){
      box(width = 12, title = strong('Interpretasi'),
          status = 'info', solidHeader = T,
          background = 'info', gradient = T,icon = icon('lightbulb'),
        column(width=12,
             column(width = 8, offset = 2,
                    h4('Interpretasi Kondisi NonMigas',
                       style='color:black;
                                text-align:center'),
                    style='background-color:lavender;
                             border-radius: 20px;
                             border:2px solid ForestGreen'),
             column(width=10, offset = 0,
                    br()),
             column(width =10, offset = 1,
                    br(),
                    p('Dalam periode tahun 1996 - 2020, nilai ekspor nonmigas
                      Indonesia selalu lebih tinggi dari nilai impor nonmigas
                      Indonesia, kecuali pada tahun 1996 dimana nilai impor
                      nonmigas Indonesia sedikit lebih tinggi daripada nilai
                      ekspor nonmigas Indonesia dengan selisih sebesar 1.246,2
                      juta USD. Secara keseluruhan, nilai ekspor dan impor nonmigas
                      Indonesia cenderung mengalami peningkatan dalam kurun waktu tersebut.',
                      style='text-indent:30px;
                               text-align:justify'),
                    p('Pada tahun 2009, terjadi penurunan baik dalam ekspor maupun
                      impor nonmigas. Hal ini disebabkan oleh krisis ekonomi yang
                      terjadi pada tahun 2008 yang menyebabkan kelesuan ekonomi di
                      berbagai negara, termasuk Indonesia. Namun, pada periode 2010-2011,
                      nilai ekspor dan impor nonmigas Indonesia meningkat drastis,
                      bahkan nilai ekspor nonmigas 2011 mencapai titik kedua tertinggi
                      dalam periode 1996 - 2020 dengan total nilai 162.019,6 juta USD.',
                      style='text-indent:30px;
                              text-align:justify'),
                    p('Pada periode 2012 - 2015, nilai ekspor dan impor nonmigas kembali
                      melesu sampai titik terendahnya di tahun 2015. Penurunan ini terjadi
                      karena berakhirnya boom komoditas primer mulai akhir tahun 2011,
                      dilanjur dengan peningkatan tingkat suku bunga oleh bank sentral
                      Amerika Serikat, lalu musim kering berkepanjangan yang menyebabkan
                      gagal panen di sentra produksi beras, dan yang terakhir adalah devaluasi
                      mini renminbi, mata uang Republik Rakyat Tiongkok.',
                      style='text-indent:30px;
                              text-align:justify'),
                    p('Setelah 2016, nilai ekspor nonmigas Indonesia cenderung meningkat
                      walau terjadi penurunan cukup signifikan pada nilai impor nonmigas
                      dikarenakan pandemic Covid-19.',
                      style='text-indent:30px;
                              text-align:justify'),
                    style='background-color:lavender;
                    border-radius: 20px;
                    align:justify;
                    color:black;
                    border:2px solid ForestGreen')
      ))
      
      #jika memilih sunburst plot nonmigas
    }else if (input$jenisPlotNonMigas=='Sunburst plot'){
      box(width = 12, title = strong('Interpretasi'),icon = icon('lightbulb'),
          status = 'info', background = 'info',
          solidHeader = T, gradient = T,
        column(width=12,
             column(width = 8, offset = 2,
                    h4('Interpretasi Sunburst plot',
                       style='color:black;
                                text-align:center'),
                    style='background-color:lavender;
                             border-radius: 20px;
                             border:2px solid ForestGreen'),
             column(width=10, offset = 0,
                    br()),
             column(width = 10, offset = 1,
                    br(),
                    p('Komoditas impor nonmigas Indonesia didominasi oleh golongan
                      bahan baku penolong dengan nilai 141.581,1 juta USD. Bahan baku
                      penolong merupakan bahan yang diperlukan untuk memenuhi proses
                      produksi yang hanya dimanfaatkan untuk waktu tertentu, misalnya
                      ketika perusahaan ingin meningkatkan efisiensi dalam sebuah produksi.
                      Dari 9 kategori bahan baku penolong, bahan baku (olahan) untuk industri
                      menyumbang nilai impor tertinggi sebesar 66.407,2 juta USD. Hal ini
                      menunjukkan bahwa pasokan bahan baku dari dalam negeri belum memenuhi
                      ketentuan dari sisi standar atau jenis, spesifikasi, maupun skala ekonomi.',
                      style='text-indent:30px;
                               text-align:justify'),
                    p('Jika dilihat secara keseluruhan, kategori barang modal kecuali alat
                      angkutan dari golongan barang modal menempati posisi kedua tertinggi
                      penyumbang nilai impor nonmigas Indonesia sebesar 25.934,9 juta USD.
                      Barang modal yang dimaksud disini adalah barang tahan lama yang digunakan
                      dalam produksi barang atau jasa, seperti mesin dan alat elektronik.',
                      style='text-indent:30px;
                              text-align:justify'),
                    p('Golongan barang konsumsi menyumbang nilai paling kecil dalam nilai
                      impor nonmigas Indonesia, hanya sebesar 17.181,5 juta USD. Dari sini
                      dapat terlihat bahwa industri Indonesia sudah mampu menghasilkan mayoritas
                      final goods yang diperlukan masyarakat Indonesia, sehingga tidak terlalu
                      tergantung dengan pasokan impor barang konsumsi dari luar negeri.',
                      style='text-indent:30px;
                              text-align:justify'),
                    style='background-color:lavender;
                    border-radius: 20px;
                    align:justify;
                    color:black;
                    border:2px solid ForestGreen')
      ))
      
      #jika memilih untuk map plot
    }else if (input$jenisPlotNonMigas=='Map plot'){
      #harus input jenisMapPlot dulu
      req(input$jenisMapPlotNonMigas)
      
      #jika memilih data ekspor NonMigas
      if (input$jenisMapPlotNonMigas=='ekspor'){
        box(width = 12,title = strong('Interpretasi'),
            status = 'info', background = 'info',icon = icon('lightbulb'),
            solidHeader = T, gradient = T,
          column(width=12,
               column(width = 8, offset = 2,
                      h4('Interpretasi Map plot ekspor NonMigas',
                         style='color:black;
                                text-align:center'),
                      style='background-color:lavender;
                             border-radius: 20px;
                             border:2px solid ForestGreen'),
               column(width=10, offset = 0,
                      br()),
               column(width = 10, offset = 1,
                      br(),
                      p('Dari map plot ekspor nonmigas, terlihat bahwa negara tujuan utama
                        ekspor nonmigas Indonesia adalah negara-negara di Benua Asia. Dari
                        tahun ke tahun, nilai total ekspor nonmigas Indonesia cenderung menurun
                        tipis, dengan nilai 154.940,8 juta USD pada tahun 2020 yang mana
                        mengalami penurunan sebesar 4,85% dibandingkan tahun 2018.',
                        style='text-indent:30px;
                               text-align:justify'),
                      p('Selama periode 2018 - 2020, Cina selalu menjadi importir barang
                        nonmigas Indonesia dengan nilai terbesar. Nilainya dari tahun ke
                        tahun cenderung meningkat, dan puncaknya pada tahun 2020, yaitu sebesar
                        29.936,4 juta USD atau sama dengan 19,32% dari nilai keseluruhan ekspor
                        nonmigas Indonesia.',
                        style='text-indent:30px;
                              text-align:justify'),
                      style='background-color:lavender;
                    border-radius: 20px;
                    align:justify;
                    color:black;
                    border:2px solid ForestGreen')
        ))
        
      #jika memilih data impor nonmigas
      }else{
        box(width = 12,title = strong('Interpretasi'),
            status = 'info', background = 'info',icon = icon('lightbulb'),
            solidHeader = T, gradient = T,
          column(width=10,
               column(width = 8, offset = 2,
                      h4('Interpretasi Map plot impor NonMigas',
                         style='color:black;
                                text-align:center'),
                      style='background-color:lavender;
                             border-radius: 20px;
                             border:2px solid ForestGreen'),
               column(width=10, offset = 0,
                      br()),
               column(width = 10, offset = 1,
                      br(),
                      p('Dari map plot impor nonmigas, terlihat bahwa negara asal utama
                        impor nonmigas Indonesia adalah negara-negara di Benua Asia.
                        Dari tahun ke tahun, nilai total impor nonmigas Indonesia cenderung
                        menurun, dengan nilai 127.312 juta USD pada tahun 2020 yang mana
                        mengalami penurunan sebesar 19,85% dibandingkan tahun 2018.',
                        style='text-indent:30px;
                               text-align:justify'),
                      p('Selama periode 2018 - 2020, Cina selalu menjadi eksportir barang
                        nonmigas Indonesia dengan nilai terbesar. Namun, nilainya dari tahun
                        ke tahun cenderung menurun tipis. Pada tahun 2020, nilai impor nonmigas
                        dari Cina adalah 39.353,3 juta USD atau sama dengan 30,91% dari nilai
                        keseluruhan ekspor nonmigas Indonesia. Penurunan pada tahun 2020, yaitu
                        sebesar 13,03% dari tahun 2020.',
                        style='text-indent:30px;
                              text-align:justify'),
                      style='background-color:lavender;
                    border-radius: 20px;
                    align:justify;
                    color:black;
                    border:2px solid ForestGreen')
        ))
      }
      
    }
    
  })
  
  ####Batas visualisasi nonmigas
  
  output$UIjenisData<-renderUI({
    if (input$cariData=='nonmigas'){
      radioButtons(inputId = 'acuanDataNonMigas',
                   label = 'Pilih jenis datanya',
                   choices = c('Menurut Negara',
                               'Per tahun',
                               'Menurut golongan'))
    #berarti memilih migas
    }else{
      selectInput(inputId = 'dataKategoriMigas',
                  label = 'Pilih data kategori Migas',
                  choices = c('Minyak mentah',
                              'Hasil minyak',
                              'Gas'))
    }
  })
  
  output$UIacuanData<-renderUI({
    if (input$cariData=='migas'){
      radioButtons(inputId = 'acuanData',
                   label = 'Pilih jenis datanya',
                   choices = c('Menurut Negara',
                               'Per tahun'))
    }
  })
  
  output$UItabelInterest<-renderUI({
    
    req(input$cariData)
    if (input$cariData=='migas'){
      if (input$acuanData=='Menurut Negara'&
          input$dataKategoriMigas != 'Minyak mentah' &
          input$kegiatanData == 'impor'){
        tags$img(src='DataNotFound.png')
      }else{
        tableOutput(outputId = 'tabelInterest') 
      }
    #berarti nonmigas
    }else{
      if (input$kegiatanData=='ekspor' &
          input$acuanDataNonMigas=='Menurut golongan'){
        tags$img(src='DataNotFound.png')
      }else{
      tableOutput(outputId = 'tabelInterest')
      }
    }
  })
  
  output$tabelInterest<-renderTable({
    
    req(input$cariData)
    #jika memilih migas
    if (input$cariData=='migas'){
      #jika memilih menurut negara
      if (input$acuanData=='Menurut Negara'){
        #berarti memilih ekspor migas menurut negara
        if (input$kegiatanData=='ekspor'){
          dataInterest<-switch(input$dataKategoriMigas,
                              'Minyak mentah'=EksporMinyakMentah,
                              'Hasil minyak'=EksporHasilMinyak,
                              'Gas'=EksporGas)
          dataInterest <- dataInterest %>%
            select(-meanLong,
                   -meanLat)
        #berarti memilih import migas menurut negara
        }else{
          #datanya yang ada hanya minyak mentah
          dataInterest<-ImporMinyakMentah %>%
            select(-meanLong,
                   -meanLat)
        }
      #berarti datanya migas menurut tahun
      }else{
        kolom<-paste(switch(input$dataKategoriMigas,
                            'Minyak mentah'='MinyakMentah',
                            'Hasil minyak'='HasilMinyak',
                            'Gas'='Gas'),
                     switch(input$kegiatanData,
                            'ekspor'='Ekspor',
                            'impor'='Impor'),
                     sep='')
        dataInterest<-dataNilaiMigas %>%
          select(-frame) %>%
          distinct()
        dataInterest$Tahun<-as.integer(dataInterest$Tahun)
        dataInterest<-dataInterest[ , c('Tahun',kolom)]
      }
    #berarti datanya nonmigas
    }else{
      #jika ekspor
      if (input$kegiatanData=='ekspor'){
        #jika memilih ekspor nonmigas menurut negara
        if(input$acuanDataNonMigas=='Menurut Negara'){
          dataInterest<-NilaiEksporNonMigasNegara %>%
            select(-meanLat,
                   -meanLong)
        #jika memilih ekspor nonmigas per tahun
        }else if (input$acuanDataNonMigas=='Per tahun'){
          dataInterest<-dataNilaiNonMigas %>%
            select(-frame) %>%
            select(Tahun, NonMigasEkspor) %>%
            distinct()
          dataInterest$Tahun<-as.integer(dataInterest$Tahun)
        }
      #berarti memilih impor
      }else{
        #Jika memilih impor nonmigas menurut negara
        if(input$acuanDataNonMigas=='Menurut Negara'){
          dataInterest<-NilaiImporNonMigasNegara %>%
            select(-meanLat,
                   -meanLong)
        #jika memilih impor nonmigas menurut golongan
        }else if (input$acuanDataNonMigas=='Menurut golongan'){
          dataInterest<-rbind(imporNonMigasBarangModal,
                              imporNonMigasBarangKonsumsi,
                              imporNonMigasBahanPenolong)
        }else{
          dataInterest<-dataNilaiNonMigas%>%
            select(-frame) %>%
            select(Tahun, NonMigasImpor) %>%
            distinct()
          dataInterest$Tahun<-as.integer(dataInterest$Tahun)
        }
      }
    }
    
    return(head(dataInterest,
         n=input$banyakBaris))
  },striped = T,hover = T,bordered = T,digits = 2)

}


shinyApp(ui=ui, server=server)