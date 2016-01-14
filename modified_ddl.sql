-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema cmpe220
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema cmpe220
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `cmpe220` DEFAULT CHARACTER SET utf8 ;
USE `cmpe220` ;

-- -----------------------------------------------------
-- Table `cmpe220`.`location`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `cmpe220`.`location` (
  `locationID` INT(11) NOT NULL AUTO_INCREMENT,
  `location` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`locationID`))
ENGINE = InnoDB
AUTO_INCREMENT = 13
DEFAULT CHARACTER SET = utf8;


-- -----------------------------------------------------
-- Table `cmpe220`.`readerinfo`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `cmpe220`.`readerinfo` (
  `readerID` INT(11) NOT NULL AUTO_INCREMENT,
  `readerNo` INT(11) NOT NULL,
  `locationID` INT(11) NOT NULL,
  PRIMARY KEY (`readerID`),
  INDEX `loc_id_idx` (`locationID` ASC),
  CONSTRAINT `loc_id`
    FOREIGN KEY (`locationID`)
    REFERENCES `cmpe220`.`location` (`locationID`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
AUTO_INCREMENT = 13
DEFAULT CHARACTER SET = utf8;


-- -----------------------------------------------------
-- Table `cmpe220`.`taginfo`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `cmpe220`.`taginfo` (
  `tagID` INT(11) NOT NULL AUTO_INCREMENT,
  `tagNo` VARCHAR(100) NOT NULL,
  PRIMARY KEY (`tagID`))
ENGINE = InnoDB
AUTO_INCREMENT = 11
DEFAULT CHARACTER SET = utf8;


-- -----------------------------------------------------
-- Table `cmpe220`.`trackinfo`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `cmpe220`.`trackinfo` (
  `trackID` INT(11) NOT NULL AUTO_INCREMENT,
  `readerID` INT(11) NOT NULL,
  `tagID` INT(11) NOT NULL,
  `actionDate` DATE NOT NULL,
  `captureTime` TIME NOT NULL,
  PRIMARY KEY (`trackID`),
  INDEX `reader_id_idx` (`readerID` ASC),
  INDEX `tag_id_track_idx` (`tagID` ASC),
  CONSTRAINT `reader_id`
    FOREIGN KEY (`readerID`)
    REFERENCES `cmpe220`.`readerinfo` (`readerID`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `tag_id_track`
    FOREIGN KEY (`tagID`)
    REFERENCES `cmpe220`.`taginfo` (`tagID`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
AUTO_INCREMENT = 51
DEFAULT CHARACTER SET = utf8;


-- -----------------------------------------------------
-- Table `cmpe220`.`userinfo`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `cmpe220`.`userinfo` (
  `userID` INT(11) NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(255) NOT NULL,
  `age` INT(11) NULL DEFAULT NULL,
  `gender` VARCHAR(45) NOT NULL,
  `phone` VARCHAR(45) NOT NULL,
  `address` TEXT NULL DEFAULT NULL,
  `occupation` VARCHAR(255) NOT NULL,
  `tagID` INT(11) NOT NULL,
  `status` VARCHAR(45) NOT NULL,
  PRIMARY KEY (`userID`),
  INDEX `tag_id_idx` (`tagID` ASC),
  CONSTRAINT `tag_id`
    FOREIGN KEY (`tagID`)
    REFERENCES `cmpe220`.`taginfo` (`tagID`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB
AUTO_INCREMENT = 11
DEFAULT CHARACTER SET = utf8;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
