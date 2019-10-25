report yteste .


class miro_similation definition .

  public section .

    types:

      begin of ty_ekp,
        ebeln   type ebeln,
        ebelp   type ebelp,
        menge   type j_1bnetqty,
        itmnum  type j_1bitmnum,
        subitem type zlit_reci_e_subitem,
        refitm  type j_1brefitm,
      end of ty_ekp,

*     zlit_reci_s_ekp_tab type table of zlit_reci_s_ekp .
      ekp_tab type table of ty_ekp.

    class-methods simulate "Simular geracao de Nota Fiscal
      importing
        !po_items  type miro_similation=>ekp_tab
      exporting
        !j_1bnflin type j_1bnflin_tab
        !j_1bnfstx type j_1bnfstx_tab
        !return    type bapiret2_tab .

  protected section .

  private section .

    data:
      nfenum type j_1bnfdoc-nfenum value '123456789',
      series type j_1bnfdoc-nfenum value '123'.

*   Este metodo esta sendo feito como se fosse apenas para uma
*   Pedido de Compras, mas sera alterado para contemplar varios

    class-methods get_po_data
      importing
        !po_items type miro_similation=>ekp_tab
      changing
        !ekko     type ekko
        !ekpo     type me_ekpo
        !ekkn     type me_ekkn
        !return   type bapiret2_tab .

    class-methods initialization . " Limpar variaveis de execucao

endclass .

class miro_similation  implementation .

  method simulate .

    data:
      ekko        type ekko,
      ekpo_tab    type me_ekpo,
      ekkn_tab    type me_ekkn,
      return_line type bapiret2.

    miro_similation=>initialization( ) .

    if ( lines( po_items ) eq 0 ) .

      return_line-type       = 'E' .
      return_line-id         = '>0' .
      return_line-number     = '000' .
      return_line-message_v1 = 'Favor informar Pedidos de Compras.' .
      append return_line to return .
      clear  return_line .

    else .

      miro_similation=>get_po_data(
        exporting
          po_items = po_items
        changing
          ekko     = ekko
          ekpo     = ekpo_tab
          ekkn     = ekkn_tab
          return   = return
      ) .

    endif .


  endmethod .

  method initialization .

*    refresh:
*    free:

  endmethod .

  method get_po_data .

    data:
      line type miro_similation=>ty_ekp .

    if ( lines( po_items ) eq 0 ) .
    else .

      read table po_items into line index 1 .

      if ( sy-subrc eq 0 ) .

        select single *
          from ekko
          into ekko
         where ebeln eq line-ebeln .

        if ( sy-subrc eq 0 ) .

          select *
            from ekpo
            into table ekpo
           where ebeln eq line-ebeln .

          if ( sy-subrc eq 0 ) .

            select *
              from ekkn
              into table ekkn
               for all entries in ekpo
             where ebeln eq ekpo-ebeln
               and ebelp eq ekpo-ebelp .

          endif .

        endif .

      endif .

    endif .

  endmethod .

endclass .

initialization .

start-of-selection .

  data:
    pedido_de_compras type miro_similation=>ekp_tab,
    nf_itens          type j_1bnflin_tab,
    nf_impostos       type j_1bnfstx_tab.

  miro_similation=>simulate(
    exporting
      po_items = pedido_de_compras
    importing
      j_1bnflin = nf_itens
      j_1bnfstx = nf_impostos
  ).


*
*  FUNCTION zlit_reci_simula_miro.
**"----------------------------------------------------------------------
**"*"Interface local:
**"  IMPORTING
**"     VALUE(I_NFENUM) TYPE  J_1BNFNUM9 DEFAULT '123456789'
**"     VALUE(I_SERIES) TYPE  J_1BSERIES DEFAULT '123'
**"  EXPORTING
**"     VALUE(E_BNFDOC) TYPE  J_1BNFDOC
**"  TABLES
**"      IT_EKP STRUCTURE  ZLIT_RECI_S_EKP OPTIONAL
**"      ET_BNFLIN TYPE  TY_J_1BNFLIN
**"      ET_BNFSTX TYPE  TY_J_1BNFSTX
**"----------------------------------------------------------------------
*
*    CLEAR: e_bnfdoc, eg_rbkpv, eg_ydrseg.
*    REFRESH: et_bnflin[], et_bnfstx[], tg_ekpo[], tg_ekkn[], tg_ydrseg[], tg_bset[],
*             tg_acchd[], tg_accit[], tg_acccr[], tg_accfi[], tg_accbset[], tg_accit_wt[],
*             tg_curr[], tg_ekbe[], tg_ekbz[], tg_ekbe_cr[], tg_ekbz_cr[], tg_vekpo[].
*
*    CHECK NOT it_ekp[] IS INITIAL.
*
** Dados pedido
*    READ TABLE it_ekp INTO eg_ekp INDEX 1.
*
**  SELECT SINGLE * FROM ekko INTO eg_ekko WHERE ebeln = i_ebeln.
*    SELECT SINGLE * FROM ekko INTO eg_ekko WHERE ebeln = eg_ekp-ebeln.
*
*    CHECK sy-subrc = 0.
*
**  SELECT SINGLE * FROM ekpo INTO eg_ekpo WHERE ebeln = i_ebeln AND ebelp = i_ebelp.
*    SELECT * FROM ekpo
*           INTO TABLE tg_ekpo
*           FOR ALL ENTRIES IN it_ekp
*           WHERE ebeln = it_ekp-ebeln
*             AND ebelp = it_ekp-ebelp.
*
**  SELECT * FROM ekkn INTO TABLE tg_ekkn  WHERE ebeln = i_ebeln AND ebelp = i_ebelp.
*    SELECT * FROM ekkn
*           INTO TABLE tg_ekkn
*           FOR ALL ENTRIES IN it_ekp
*           WHERE ebeln = it_ekp-ebeln
*             AND ebelp = it_ekp-ebelp.
*
** Busca plano de contas
*    SELECT SINGLE ktopl FROM t001
*        INTO vg_ktopl
*        WHERE bukrs EQ eg_ekko-bukrs.
*
**Busca contabilidade de custos
*    CALL FUNCTION 'RK_KOKRS_FIND'
*      EXPORTING
*        bukrs                  = eg_ekko-bukrs
*      IMPORTING
*        kokrs                  = vg_kokrs
*      EXCEPTIONS
*        assignment_not_allowed = 1
*        insufficient_input     = 2
*        no_kokrs_assigned      = 3
*        no_kokrs_for_bukrs     = 4
*        no_kokrs_for_bu_gb     = 5
*        wrong_kokrs_for_bukrs  = 6
*        wrong_kokrs_for_bu_gb  = 7
*        OTHERS                 = 8.
*
***Busca Código de agrupamento de avaliação
**  CALL FUNCTION 'CO_TA_T001K_READ'
**    EXPORTING
**      t001w_bwkey = eg_ekpo-werks
**    IMPORTING
**      t001kwa     = eg_t001k
**    EXCEPTIONS
**      not_found   = 1
**      OTHERS      = 2.
*
**Busca tipo de documento contábil padrão
*    CALL FUNCTION 'MM_T169F_READ'
*      EXPORTING
*        i_tcode = 'MIRO'
*      IMPORTING
*        e_t169f = eg_t169f.
*
**-----------Busca de dados - Fim
*
**-------------Montagem de dados - Início
*
**-----Cabeçalho
*
*    eg_rbkpv-budat    = sy-datum.  "p_dtnf.         "Data da NF
*    CONCATENATE i_nfenum '-' i_series INTO eg_rbkpv-xblnr.
*
**  eg_rbkpv-rmwwr    = p_nftot.        "Valor total da NF-e
*
*    "Categoria de NF
*    "Busca em tabela parametrizada...
*    SELECT SINGLE nftype
*           INTO eg_rbkpv-j_1bnftype
*           FROM zlit_reci_t_main
*           WHERE bukrs = eg_ekko-bukrs.
*
*    eg_rbkpv-wwert    = sy-datum.       "Data para conversão de moeada estrangeira
*    eg_rbkpv-cpudt    = sy-datum.       "data do dia
*    eg_rbkpv-bldat    = sy-datum.       "Data da entrada
*    eg_rbkpv-repdat   = sy-datum.       "data da revisão de fatura
*    eg_rbkpv-gjahr    = sy-datum(4).    "Ano do lançamento
*    eg_rbkpv-usnam    = sy-uname.       "Usuário para perfil de acesso
*    eg_rbkpv-erfnam   = sy-uname.       "Usuário para perfil de acesso
*    eg_rbkpv-mandt    = sy-mandt.       "mandante
*    eg_rbkpv-saprl    = sy-saprl.       "Release SAP
*    eg_rbkpv-cputm    = sy-uzeit.       "hora da entrada
*    eg_rbkpv-reptim   = sy-uzeit.       "hora da revisão de fatura
*
*    eg_rbkpv-blart    = eg_t169f-blart. "tipo de eocumento contábil
*
*    eg_rbkpv-tcode    = 'MIRO'.         "Transação - MIRO para evitar problemas nas validações
*
*    eg_rbkpv-vgart    = 'RD'.           "Tipo de operação em AG08 (tp.interno de documento)
*    eg_rbkpv-xrechl   = 'S'.            "Lógica de lançamento para itens remessa (fatura/nota créd.)
*    eg_rbkpv-xrechr   = 'H'.            "Lógica de lançamento para itens devolução (fat./nota cré
*    eg_rbkpv-xrech    = 'X'.            "Código: registrar fatura
*    eg_rbkpv-xmwst    = 'X'.            "Calcular imposto automaticamente?
*    eg_rbkpv-xzuordli = 'X'.            "Código: atribuição itens de remessa
*    eg_rbkpv-xzuordrt = 'X'.            "Código: atribuição de devoluções
*    eg_rbkpv-xware    = 'X'.            "Código: fatura de mercadorias/serviço
*    eg_rbkpv-xbest    = 'X'.            "Campo de seleção: atribuições
*    eg_rbkpv-xkorrekt = 'X'.            "Código que indica se a fatura está correta
*    eg_rbkpv-ivtyp    = 'B'.            "Lançamento por BAPI
*
**IVA Cabeçalho
*    eg_rbkpv-xrbtx    = 'X'.            "Códigos a ser preenchido caso os itens tenham IVA´s diferenras
*    eg_rbkpv-mwskz1   = eg_ydrseg-mwskz."IVA do cabeçalho
*
**Dados do pedido
*    eg_rbkpv-kursf = eg_ekko-wkurs.     "Taxa de cambio
*    eg_rbkpv-txkrs = eg_ekko-wkurs.     "Taxa de cambio
*    eg_rbkpv-lifnr = eg_ekko-lifnr.     "Fornecedor
*    eg_rbkpv-bukrs = eg_ekko-bukrs.     "Empresa
*    eg_rbkpv-waers = eg_ekko-waers.     "Moeda do documento
*
**Condição de pagamento
*    eg_rbkpv-zterm = eg_ekko-zterm.     "Condição de pagamento
*    CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
*      EXPORTING
*        i_bldat         = sy-datum
*        i_budat         = sy-datum
*        i_cpudt         = sy-datum
*        i_zfbdt         = sy-datum
*        i_zterm         = eg_ekko-zterm
*      IMPORTING
*        e_zbd1t         = eg_rbkpv-zbd1t
*        e_zbd1p         = eg_rbkpv-zbd1p
*        e_zbd2t         = eg_rbkpv-zbd2t
*        e_zbd2p         = eg_rbkpv-zbd2p
*        e_zbd3t         = eg_rbkpv-zbd3t
*        e_zfbdt         = eg_rbkpv-zfbdt
*        e_zlsch         = eg_rbkpv-zlsch
*      EXCEPTIONS
*        terms_not_found = 1
*        OTHERS          = 2.
*
**Sistema Lógico
*    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
*      IMPORTING
*        own_logical_system = eg_rbkpv-logsys.
*
*    "Quantidade de itens na tg_ydrseg.
*    LOOP AT it_ekp INTO eg_ekp.
*      ADD 1 TO eg_rbkpv-anzrpo.
*    ENDLOOP.
*
**-----Item
*    LOOP AT tg_ekpo INTO eg_ekpo.
*
*      IF sy-tabix = 1.
*        "Busca Código de agrupamento de avaliação
*        CALL FUNCTION 'CO_TA_T001K_READ'
*          EXPORTING
*            t001w_bwkey = eg_ekpo-werks
*          IMPORTING
*            t001kwa     = eg_t001k
*          EXCEPTIONS
*            not_found   = 1
*            OTHERS      = 2.
*      ENDIF.
*
*      CLEAR vg_item.
*      LOOP AT it_ekp INTO eg_ekp WHERE ebeln = eg_ekpo-ebeln
*                                   AND ebelp = eg_ekpo-ebelp.
*
*        ADD 1 TO vg_item.
*
*        eg_ekp-refitm = vg_item.
*        MODIFY it_ekp FROM eg_ekp INDEX sy-tabix TRANSPORTING refitm.
*
**    eg_ydrseg-rblgp             = '1'.             "Item de documento no documento de faturamento - Deve ser sequencial por item da rseg
*        eg_ydrseg-rblgp             = vg_item.             "Item de documento no documento de faturamento - Deve ser sequencial por item da rseg
*        eg_ydrseg-enqueue_granted   = 'X'.             "variável booleana (X=verdade,
*        eg_ydrseg-selkz             = 'X'.             "Código de seleção
*        eg_ydrseg-selkz             = 'X'.             "Código de seleção
*        eg_ydrseg-xbesw             = 'X'.             "Código de atualização em moeda do pedido
*        eg_ydrseg-selkz             = 'X'.             "Código de seleção
*        eg_ydrseg-xekbe             = 'X'.             "Código: atualizar histórico do pedido
*        eg_ydrseg-shkzg             = 'S'.             "Código débito/crédito
*
*        "Pega a quantidade atribuída ----------------------------------------
*        CLEAR eg_ekp.
*        READ TABLE it_ekp INTO eg_ekp WITH KEY ebeln = eg_ekpo-ebeln
*                                             ebelp = eg_ekpo-ebelp.
*
*        IF eg_ekp-menge > 0.
*          eg_ekpo-netwr = ( eg_ekpo-netwr / eg_ekpo-menge ) * eg_ekp-menge.
*          eg_ekpo-menge = eg_ekp-menge.
*        ENDIF.
*        "--------------------------------------------------------------------
*
*        eg_ydrseg-wenam             = sy-uname.        "Entrada de mercadorias criada por
*        eg_ydrseg-ernam             = sy-uname.        "Nome do responsável que adicionou o objeto
*
*        eg_ydrseg-eindt             = eg_rbkpv-budat.  "Data de remessa do item
*        eg_ydrseg-webud             = eg_rbkpv-budat.  "Data de lançamento entrada e mercadoria
*        eg_ydrseg-bwmod             = eg_t001k-bwmod. "Código de agrupamento de avaliação
*
*        eg_ydrseg-kokrs             = vg_kokrs.        "Área de contabilidade de custos
*        eg_ydrseg-ktopl             = vg_ktopl.        "Plano de contas
*
*        eg_ydrseg-bedat             = eg_ekko-bedat.     "Data do pedido
*        eg_ydrseg-bukrs             = eg_ekko-bukrs.     "Empresa
*        eg_ydrseg-ekgrp             = eg_ekko-ekgrp.     "Grupo de compradores
*        eg_ydrseg-inco1             = eg_ekko-inco1.     "Incoterms parte 1
*        eg_ydrseg-inco2             = eg_ekko-inco2.     "Incoterms parte 2
*        eg_ydrseg-knumvk            = eg_ekko-knumv.     "Nº condição do documento
*        eg_ydrseg-bewae             = eg_ekko-waers.     "Moeda do pedido
*        eg_ydrseg-hwaer             = eg_ekko-waers.     "Moeda interna
*        eg_ydrseg-waers             = eg_ekko-waers.     "Código da moeda
*        eg_ydrseg-bwaer             = eg_ekko-waers.     "Código da moeda
*        eg_ydrseg-hswae             = eg_ekko-waers.     "Chave de moeda interna
*        eg_ydrseg-kursf             = eg_ekko-wkurs.     "Taxa de câmbio
*        eg_ydrseg-pstyp             = eg_ekpo-pstyp.     "Categoria de item do pedido
*        eg_ydrseg-bprme             = eg_ekpo-bprme.     "Unidade do preço do pedido
*        eg_ydrseg-bpumn             = eg_ekpo-bpumn.     "Denominador para a conversão UMPP em UMP
*        eg_ydrseg-bpumz             = eg_ekpo-bpumz.     "Numerador para a conversão UMPP em UMP
*        eg_ydrseg-ebeln             = eg_ekpo-ebeln.     "Nº pedido
*        eg_ydrseg-ebelp             = eg_ekpo-ebelp.     "Nº item do documento de compra
*        eg_ydrseg-ebonf             = eg_ekpo-ebonf.     "Item não relevante para liquidação posterior
*        eg_ydrseg-inco1             = eg_ekpo-inco1.     "Incoterms parte 1
*        eg_ydrseg-inco2             = eg_ekpo-inco2.     "Incoterms parte 2
*        eg_ydrseg-knttp             = eg_ekpo-knttp.     "Categoria de classificação contábil
*        eg_ydrseg-basme             = eg_ekpo-lmein.     "Unidade de medida básica
*        eg_ydrseg-matkl             = eg_ekpo-matkl.     "Grupo de mercadorias
*        eg_ydrseg-matnr             = eg_ekpo-matnr.     "Nº do material
*        eg_ydrseg-matbf             = eg_ekpo-matnr.     "Material com base no qual se administra o estoque
*        eg_ydrseg-meins             = eg_ekpo-meins.     "Unidade de medida do pedido
*        eg_ydrseg-mtart             = eg_ekpo-mtart.     "Tipo de material
*        eg_ydrseg-mwskz             = eg_ekpo-mwskz.     "Código do IVA
*        eg_ydrseg-peinh             = eg_ekpo-peinh.     "Unidade de preço
*        eg_ydrseg-txjcd             = eg_ekpo-txjcd.     "Domicílio fiscal
*        eg_ydrseg-txz01             = eg_ekpo-txz01.     "Texto breve
*        eg_ydrseg-umren             = eg_ekpo-umren.     "Denominador para a conversão em unidades de medida básicas
*        eg_ydrseg-umrez             = eg_ekpo-umrez.     "Contador para a conversão em UMs básicas
*        eg_ydrseg-webre             = eg_ekpo-webre.     "Código p/revisão de faturas baseado na entrada mercadorias
*        eg_ydrseg-wepos             = eg_ekpo-wepos.     "Código de entrada de mercadorias
*        eg_ydrseg-werks             = eg_ekpo-werks.     "Centro
*        eg_ydrseg-bwkey             = eg_ekpo-werks.     "Área de avaliação
*
**Bloco valores e quantidades, este bloco deve ser montado conforme valores do XML e não do pedido
**Eventuais diferenças devem ter limites de tolerância no standard.
**Neste caso devem ser verificado também se os impostos estão incluidos no preço (Tabela J_1BKON1)
*
***Inicio
*        eg_ydrseg-netpr      = eg_ekpo-netpr.     "Preço líquido
*
*        eg_ydrseg-wewrt      = eg_ekpo-effwr.     "Valor da entrada de mercadorias em moeda interna
*        eg_ydrseg-wewwr      = eg_ekpo-effwr.     "Valor de entrada de mercadorias em moeda estrangeira
*        eg_ydrseg-wewrb      = eg_ekpo-effwr.     "Valor da entrada de mercadorias em moeda do pedido
*
*        eg_ydrseg-wrbtr      = eg_ekpo-netwr.     "Montante em moeda do documento
*        eg_ydrseg-wrbtralt   = eg_ekpo-netwr.     "Montante em moeda do documento
*        eg_ydrseg-netwr      = eg_ekpo-netwr.     "Valor líquido do pedido em moeda do documento
*        eg_ydrseg-wrbtr      = eg_ekpo-netwr.     "Montante em moeda do documento
*
*        eg_ydrseg-menge      = eg_ekpo-menge.     "Quantidade
*        eg_ydrseg-mengealt   = eg_ekpo-menge.     "Quantidade
*        eg_ydrseg-bpmngalt   = eg_ekpo-menge.     "Quantidade na unidade do preço do pedido
*        eg_ydrseg-bpmng      = eg_ekpo-menge.     "Quantidade na unidade do preço do pedido
*        eg_ydrseg-bpmng      = eg_ekpo-menge.     "Quantidade na unidade do preço do pedido
*        eg_ydrseg-bsmng_f    = eg_ekpo-menge.     "Parte da quantidade linha de classificação contábil p/item
*        eg_ydrseg-wemng_f    = eg_ekpo-menge.     "Parte da quantidade linha de classificação contábil p/item
*        eg_ydrseg-bpwem_f    = eg_ekpo-menge.     "Parte da quantidade linha de classificação contábil p/item
*        eg_ydrseg-menge      = eg_ekpo-menge.     "Quantidade
**Dados complementares
**      eg_rbkpv-anzrpo             = '1'.      "Quantidade de itens na tg_ydrseg.
*
***Fim
*
**Classificação contábil
*        SORT tg_ekkn BY zekkn.
*
*        DESCRIBE TABLE tg_ekkn LINES sy-tfill.
*
*        IF sy-tfill > 1.
*          "Classificação contábil múltipla
*          LOOP AT tg_ekkn INTO eg_ekkn WHERE ebelp = eg_ekpo-ebelp.
*
*            eg_co-wrbtr    = eg_ydrseg-netwr * eg_ekkn-vproz / 100.
*            eg_co-aufnr    = eg_ekkn-aufnr.
*            eg_co-kokrs    = eg_ekkn-kokrs.
*            eg_co-kostl    = eg_ekkn-kostl.
*            eg_co-prctr    = eg_ekkn-prctr.
*            eg_co-saknr    = eg_ekkn-sakto.
*            eg_co-txjcd    = eg_ekpo-txjcd.
*            eg_co-menge    = eg_ydrseg-menge * eg_ekkn-vproz / 100.
*            eg_co-mwskz    = eg_ekpo-mwskz.
*            eg_co-zekkn    = eg_ekkn-zekkn.
*            eg_co-bukrs    = eg_ekpo-bukrs.
*            eg_co-shkzg    = 'S'.
*            eg_co-meins    = eg_ekpo-meins.
*            eg_co-werks    = eg_ekpo-werks.
*            eg_co-wemng    = eg_ydrseg-menge.
*            eg_co-bpwem    = eg_ydrseg-menge.
*            eg_co-selkz    = 'X'.
*
*            APPEND eg_co TO eg_ydrseg-co.
*
*          ENDLOOP.
*
*        ELSEIF sy-tfill > 0.
*
*          "Classificação contábil simples
*          READ TABLE tg_ekkn INTO eg_ekkn WITH KEY ebeln = eg_ekpo-ebeln
*                                                   ebelp = eg_ekpo-ebelp.
*
*          IF sy-subrc IS INITIAL.
*
*            eg_ydrseg-zekkn    = eg_ekkn-zekkn.     "Nº seqüencial da classificação contábil
*            eg_ydrseg-kostl    = eg_ekkn-kostl.     "Centro de custo
*            eg_ydrseg-ko_prctr = eg_ekkn-prctr.     "Centro de lucro
*            eg_ydrseg-prctr    = eg_ekkn-prctr.     "Centro de lucro
*            eg_ydrseg-saknr    = eg_ekkn-sakto.     "Nº conta do Razão
*
*          ENDIF.
*
*        ENDIF.
*
*        APPEND eg_ydrseg TO tg_ydrseg.
*
*      ENDLOOP.
*
*    ENDLOOP.
*
**Custos complementares planejados
**  PERFORM fill_planned_costs.
*
**Completar dados contábeis da tg_ydrseg
*    LOOP AT tg_ydrseg.
*
*      CALL FUNCTION 'MRM_DRSEG_COMPLETE_MAT_DATA'
*        EXPORTING
*          i_drseg        = tg_ydrseg
*          i_kzspr        = ''
*          i_trtyp        = ' '
*          i_smbew_modify = ' '
*        IMPORTING
*          e_drseg        = tg_ydrseg
*        EXCEPTIONS
*          error_message  = 01.
*
*      MODIFY tg_ydrseg.
*
*    ENDLOOP.
*
**-------------Montagem de dados - Fim
*
**-------------Execução do processo de simulação - Início
*
**Ativar origem revisão de faturas
**Necessário chamar antes da J_1B_NF_IV_MATKL_DETERMINE para carregar memória do grupo de função
*    CALL FUNCTION 'J_1B_SET_TAX_CALLER'
*      EXPORTING
*        i_caller = 'IV'.
*
** Set invoice verification simulation mode yes/no
**Necessário chamar antes da J_1B_NF_IV_MATKL_DETERMINE para carregar memória do grupo de função
*    CALL FUNCTION 'J_1B_NFE_SET_IV_MODE'
*      EXPORTING
*        i_simulate = 'X'. "i_simulate.
*
**Carrega tabelas e estruturas das notas fiscais
**Necessário chamar antes da MR_CALCULATE_TAX_DOCUMENT para carregar memória do grupo de função
*    CALL FUNCTION 'J_1B_NF_IV_MATKL_DETERMINE'
*      EXPORTING
*        i_rbkp   = eg_rbkpv
*        i_ebeln  = eg_ekko-ebeln
*      TABLES
*        i_rseg   = tg_ydrseg
*      EXCEPTIONS
*        not_rele = 1
*        OTHERS   = 2.
*
**calcula impostos
*    CALL FUNCTION 'MR_CALCULATE_TAX_DOCUMENT'
*      EXPORTING
*        i_bukrs       = eg_rbkpv-bukrs
*        i_hswae       = 'BRL'
*        i_waers       = eg_rbkpv-waers
*        i_xmwst       = eg_rbkpv-xmwst
*        i_glvor       = 'RMRP'
*        i_budat       = eg_rbkpv-budat
*        i_kursf       = eg_rbkpv-kursf
*        i_bldat       = eg_rbkpv-bldat
*        i_zbd1p       = eg_rbkpv-zbd1p
*        i_wskto       = eg_rbkpv-wskto
*        i_rmwwr       = eg_rbkpv-rmwwr
*        i_xrech       = eg_rbkpv-xrech
*        i_txkrs       = eg_rbkpv-txkrs
*        i_wrbtr_bnk   = eg_rbkpv-beznk
*        i_mwskz_bnk   = eg_rbkpv-mwskz_bnk
*        i_txjcd_bnk   = eg_rbkpv-txjcd_bnk
*        i_t169p_bnk   = ''
*        i_lifnr       = eg_rbkpv-lifnr
*        i_wwert       = eg_rbkpv-wwert
*        i_vatdate     = eg_rbkpv-vatdate
*      IMPORTING
*        e_xfwste      = vg_po_fwste
*        e_prsdt       = vg_prsdt
*        e_navfw_bnk   = vg_po_navfw_bnk
*        e_vatdate     = eg_rbkpv-vatdate
*      TABLES
*        t_drseg       = tg_ydrseg
*        t_bset        = tg_bset
*      EXCEPTIONS
*        error_message = 01
*        OTHERS        = 02.
*
**Completa dados de cabeçalho conforme resutaldo da função MR_CALCULATE_TAX_DOCUMENT
*    eg_rbkpv-wmwst1    = vg_po_fwste. "Valor do imposto
**  eg_rbkpv-rpzieln   = p_nftot - vg_po_fwste. "Total da NF líquido
*    eg_rbkpv-rpzieln   = vg_po_fwste. "Total da NF líquido
*    eg_rbkpv-rpzielmw  = vg_po_fwste. "Valor do imposto
*
*    CALL FUNCTION 'MRM_PROT_INIT'
*      EXPORTING
*        i_xall = 'X'.
*
**Informa dados da NF-e
**Estes dados funcionam por padrão na MIGO, para a MIRO deve ser tratado em BADI.
**  EXPORT docnum9_xml FROM p_nfenum
**  TO MEMORY ID 'DOCNUM9XML'.
**
**  EXPORT authcode FROM p_atcode
**  TO MEMORY ID 'J1B_AUTHCODE'.
**
**  EXPORT  xmlvers FROM p_xmlv
**  TO MEMORY ID 'J1B_XMLVERS'.
**
**  EXPORT authdate FROM p_atdate
**  TO MEMORY ID 'J1B_AUTHDATE'.
**
**  EXPORT authtime FROM p_attime
**  TO MEMORY ID 'J1B_AUTHTIME'.
*
**Muda de variavel apenas devido ao formato do campo para a função MRM_INVOICE_POST
**  vg_rmwwr = p_nftot.
*
**Reseta log para chamada da função da MIRO
*    CALL FUNCTION 'MRM_PROT_INIT'
*      EXPORTING
*        i_xall = 'X'.
*
**simula criação da miro e nota fiscal
*    CALL FUNCTION 'MRM_INVOICE_POST'
*      EXPORTING
*        i_rbkpv                  = eg_rbkpv
*        i_skonto_netto           = eg_rbkpv-rpzieln
*        i_skonto_brutto          = vg_rmwwr
*        i_sktobas_netto          = ' '
*        i_sktobas_brutto         = ' '
*        i_skonto_bnk             = ' '
*        i_nav_bnk                = ' '
*        i_skv_nav_fw_sum         = ' '
*        i_fwste                  = eg_rbkpv-wmwst1
*        i_xfwnav                 = ' '
*        i_simulation             = 'X'
*      TABLES
*        t_drseg                  = tg_ydrseg
*        t_bset                   = tg_bset
*      EXCEPTIONS
*        no_entry_found           = 1
*        saldo_error              = 2
*        internal_error           = 3
*        number_error             = 4
*        currency_error           = 5
*        error_in_function_module = 6
*        parameter_error          = 7
*        period_error             = 8
*        no_account_found         = 9
*        error_at_message_setup   = 10
*        fatal_error              = 11
*        post_error               = 12
*        blart_missing            = 13
*        baseline_date_initial    = 14
*        cond_corr_error          = 15
*        no_onetime_vendor_data   = 16
*        date_tolerance           = 17
*        ac_document_create_error = 18
*        material_not_enqueued    = 19
*        error_in_badi            = 20
*        error_message            = 21
*        OTHERS                   = 22.
*
**Recupera erros em tela da chamada da MIRO
*    IF sy-subrc IS NOT INITIAL.
*      eg_errprot-msgid = sy-msgid.
*      eg_errprot-msgno = sy-msgno.
*      eg_errprot-msgty = sy-msgty.
*      eg_errprot-msgv1 = sy-msgv1.
*      eg_errprot-msgv2 = sy-msgv2.
*      eg_errprot-msgv3 = sy-msgv3.
*      eg_errprot-msgv4 = sy-msgv4.
*    ENDIF.
*
**Recupera log de mensagens da miro
*    CALL FUNCTION 'MRM_PROT_GET'
*      IMPORTING
*        te_errprot = tg_errprot.
*
*    IF eg_errprot IS NOT INITIAL.
*      APPEND eg_errprot TO tg_errprot.
*    ENDIF.
*
**-------------Execução do processo de simulação - Fim
*
**-------------Recuperação dos dados gerados
*
**Busca dados para simulação de contabilidade
*    CALL FUNCTION 'MRM_XACCITCR_EXPORT'
*      TABLES
*        t_acchd    = tg_acchd
*        t_accit    = tg_accit
*        t_acccr    = tg_acccr
*        t_accfi    = tg_accfi
*        t_accbset  = tg_accbset
*        t_accit_wt = tg_accit_wt
*        t_curr     = tg_curr
*        t_ekbe     = tg_ekbe
*        t_ekbz     = tg_ekbz
*        t_ekbe_cr  = tg_ekbe_cr
*        t_ekbz_cr  = tg_ekbz_cr
*        t_vekpo    = tg_vekpo.
*
**Carrega tabela de simulação de contabilidade
**  PERFORM f_simula_contabilidade USING eg_ekko-ebeln.
*
**Recupera objeto de nota fiscal
*    GET PARAMETER ID 'J_1BNFE_OBJECT' FIELD vg_nfobj.
*
**Recupera dados da nota fiscal
*    IF NOT vg_nfobj IS INITIAL.
*
*      CALL FUNCTION 'J_1B_NF_OBJECT_READ'
*        EXPORTING
*          obj_number        = vg_nfobj
*        IMPORTING
*          obj_header        = eg_obj_header
*        TABLES
*          obj_partner       = tg_obj_partner
*          obj_item          = tg_obj_item
*          obj_item_tax      = tg_obj_item_tax
*          obj_header_msg    = tg_obj_header_msg
*          obj_refer_msg     = tg_obj_refer_msg
*          obj_ot_partner    = tg_ot_partner
*          obj_import_di     = tg_import_di
*          obj_import_adi    = tg_import_adi
*          obj_cte_res       = tg_cte_res
*          obj_cte_docref    = tg_cte_docref
*          obj_trans_volumes = tg_trans_volumes
*          obj_trailer_info  = tg_trailer_info
*          obj_trade_notes   = tg_trade_notes
*          obj_add_info      = tg_add_info
*          obj_ref_proc      = tg_ref_proc
*          obj_sugar_suppl   = tg_sugar_suppl
*          obj_sugar_deduc   = tg_sugar_deduc
*          obj_vehicle       = tg_vehicle
**         obj_pharmaceut    = tg_pharmaceut
*          obj_fuel          = tg_fuel
*          obj_export        = tg_export
*          obj_nve           = tg_nve
**         obj_traceability  = tg_traceability
**         obj_pharma        = tg_pharma
**         obj_payment       = tg_payment
*        EXCEPTIONS
*          object_not_found  = 1
*          OTHERS            = 2.
*
*      e_bnfdoc    = eg_obj_header.
*      et_bnflin[] = tg_obj_item[].
*      et_bnfstx[] = tg_obj_item_tax[].
*
*    ENDIF.
*
*  ENDFUNCTION.
