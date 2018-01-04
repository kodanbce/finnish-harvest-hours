module.exports = {

    mongoUrl: process.env.MONGODB_URI || 'mongodb://localhost/saldot',
    harvestUrl: `https://${process.env.ORGANIZATION}.harvestapp.com`
}
