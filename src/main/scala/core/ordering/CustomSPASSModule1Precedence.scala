package core.ordering


/**
 * User: nowi
 * Date: 19.01.2010
 * Time: 20:58:41
 */

class CustomSPASSModule1Precedence extends Precedence {
  // create the internaly used string comperator

  // build the precedence list



  def getSymbolicNames = sortedSymbolicNames

  private lazy val sortedSymbolicNames: List[String] = {
    val precedence = List("div", "id", "tt", "skf00", "skf01", "skf02", "skf03", "skf04", "skf05", "skf06", "skf07", "skf08", "skf09", "skf010", "skf011", "skf012", "skf013", "skf014", "skf015", "skf016", "skf017", "skf018", "skf019", "skf020", "skf021", "skf022", "skf023", "skf024", "skf025", "skf026", "skf027", "skf028", "skf029", "skf030", "skf031", "skf032", "skf033", "skf034", "skf035", "skf036", "skf037", "skf038", "skf039", "skf040", "skf041", "skf042", "skf043", "skf044", "skf045", "skf046", "skf047", "skf048", "skf049", "skf050", "skf051", "skf052", "skf053", "skf054", "skf055", "skf056", "skf057", "skf058", "skf059", "skf060", "skf061", "skf062", "skf063", "skf064", "skf065", "skf066", "skf067", "skf068", "skf069", "skf070", "skf071", "skf072", "skf073", "skf074", "skf075", "skf076", "skf077", "skf078", "skf079", "skf080", "skf081", "skf082", "skf083", "skf084", "skf085", "skf086", "skf087", "skf088", "skf089", "skf090", "skf091", "skf092", "skf093", "skf094", "skf095", "skf096", "skf097", "skf098", "skf099", "skf0100", "skf0101", "skf0102", "skf0103", "skf0104", "skf0105", "skf0106", "skf0107", "skf0108", "skf0109", "skf0110", "skf0111", "skf0112", "skf0113", "skf0114", "skf0115", "skf0116", "skf0117", "skf0118", "skf0119", "skf0120", "skf0121", "skf0122", "skf0123", "skf0124", "skf0125", "skf0126", "skf0127", "skf0128", "skf0129", "skf0130", "skf0131", "skf0132", "skf0133", "skf0134", "skf0135", "skf0136", "skf0137", "skf0138", "skf0139", "skf0140", "skf0141", "skf0142", "skf0143", "skf0144", "skf0145", "skf0146", "skf0147", "skf0148", "skf0149", "skf0150", "skf0151", "skf0152", "skf0153", "skf0154", "skf0155", "skf0156", "skf0157", "skf0158", "skf0159", "skf0160", "skf0161", "skf0162", "skf0163", "skf0164", "skf0165", "skf0166", "skf0167", "skf0168", "skf0169", "skf0170", "skf0171", "skf0172", "skf0173", "skf0174", "skf0175", "skf0176", "skf0177", "skf0178", "skf0179", "skf0180", "skf0181", "skf0182", "skf0183", "skf0184", "skf0185", "skf0186", "skf0187", "skf0188", "skf0189", "skf0190", "skf0191", "skf0192", "skf0193", "skf0194", "skf0195", "skf0196", "skf0197", "skf0198", "skf0199", "skf0200", "skf0201", "skf0202", "skf0203", "skf0204", "skf0205", "skf0206", "skf0207", "skf0208", "skf0209", "skf0210", "skf0211", "skf0212", "skf0213", "skf0214", "skf0215", "Pp", "P", "Ep", "E", "NEWATOMIC7p", "NEWATOMIC7", "Fp", "F", "Hydrophobicp", "Hydrophobic", "NEWATOMIC11p", "NEWATOMIC11", "NEWATOMIC4p", "NEWATOMIC4", "Ap", "A", "LargeAliphaticAminoAcidp", "LargeAliphaticAminoAcid", "Cp", "C", "hasSideChainStructurep", "hasSideChainStructure", "NEWATOMIC29p", "NEWATOMIC29", "NEWATOMIC5p", "NEWATOMIC5", "PositiveChargedAminoAcidp", "PositiveChargedAminoAcid", "Polarp", "Polar", "NEWATOMIC2p", "NEWATOMIC2", "hasPolarityp", "hasPolarity", "NEWATOMIC0p", "NEWATOMIC0", "hasSizep", "hasSize", "NEWATOMIC22p", "NEWATOMIC22", "Hydrophobicityp", "Hydrophobicity", "NEWATOMIC10p", "NEWATOMIC10", "NEWATOMIC1p", "NEWATOMIC1", "Neutralp", "Neutral", "NEWATOMIC21p", "NEWATOMIC21", "NEWATOMIC31p", "NEWATOMIC31", "Rp", "R", "Yp", "Y", "hasChargep", "hasCharge", "Ip", "I", "Positivep", "Positive", "NEWATOMIC26p", "NEWATOMIC26", "NEWATOMIC25p", "NEWATOMIC25", "NEWATOMIC23p", "NEWATOMIC23", "AromaticAminoAcidp", "AromaticAminoAcid", "Gp", "G", "Np", "N", "NEWATOMIC3p", "NEWATOMIC3", "Largep", "Large", "hasHydrophobicityp", "hasHydrophobicity", "NEWATOMIC30p", "NEWATOMIC30", "Qp", "Q", "NEWATOMIC18p", "NEWATOMIC18", "Aliphaticp", "Aliphatic", "NEWATOMIC28p", "NEWATOMIC28", "NEWATOMIC16p", "NEWATOMIC16", "Tinyp", "Tiny", "NEWATOMIC19p", "NEWATOMIC19", "Dp", "D", "Negativep", "Negative", "Smallp", "Small", "NEWATOMIC8p", "NEWATOMIC8", "AminoAcidp", "AminoAcid", "NonPolarp", "NonPolar", "Hydrophilicp", "Hydrophilic", "Aromaticp", "Aromatic", "NEWATOMIC6p", "NEWATOMIC6", "Vp", "V", "Lp", "L", "Sp", "S", "NEWATOMIC24p", "NEWATOMIC24", "SpecificAminoAcidp", "SpecificAminoAcid", "Chargep", "Charge", "Sizep", "Size", "TinyPolarAminoAcidp", "TinyPolarAminoAcid", "NEWATOMIC27p", "NEWATOMIC27", "NEWATOMIC9p", "NEWATOMIC9", "NEWATOMIC32p", "NEWATOMIC32", "Kp", "K", "AliphaticAminoAcidp", "AliphaticAminoAcid", "RefiningFeaturep", "RefiningFeature", "Wp", "W", "Mp", "M", "Polarityp", "Polarity", "NEWATOMIC13p", "NEWATOMIC13", "Hp", "H", "SideChainStructurep", "SideChainStructure", "NEWATOMIC20p", "NEWATOMIC20", "NEWATOMIC12p", "NEWATOMIC12", "NegativeChargedAminoAcidp", "NegativeChargedAminoAcid", "NEWATOMIC15p", "NEWATOMIC15", "NEWATOMIC17p", "NEWATOMIC17", "NEWATOMIC14p", "NEWATOMIC14", "Tp", "T")
    // extract the symbolic names from the clauses , and compare with default precedence
    //val symbolicNames  = clauses.values.flatMap({_.literals}).map({literal: FOLNode => literal.symbolicName})
    // there should be no symbolic names in the clauses that are not contained in the precedence
    // TODO enable checking
    //(precedence -- symbolicNames.toList )match {
    //  case _ => precedence
    //  //case missing if(!missing.isEmpty) => throw new IllegalArgumentException("Missing symbols in precedence : %s" format (missing))
    //  //case missing if(missing.isEmpty) => precedence

    //}

    precedence

  }


}